# read_wasserportal ------------------------------------------------------------

#' Download and Read Data from wasserportal.berlin.de
#' 
#' This function downloads and reads CSV files from wasserportal.berlin.de.
#' 
#' The original timestamps (column \code{timestamps_raw} in the example below)
#' are not all plausible, e.g. "31.03.2019 03:00" appears twice! They are
#' corrected (column \code{timestamp_corr}) to represent a plausible sequence of
#' timestamps in Berlin Normal Time (UTC+01) Finally, a valid POSIXct timestamp
#' in timezone "Berlin/Europe" (UTC+01 in winter, UTC+02 in summer) is created,
#' together with the additional information on the UTC offset (column
#' \code{UTCOffset}, 1 in winter, 2 in summer).
#' 
#' @param station station number, as returned by 
#'   \code{\link{get_wasserportal_stations}}
#' @param variables vector of variable identifiers, as returned by 
#'   \code{\link{get_wasserportal_variables}}
#' @param from_date \code{Date} object (or string in format "yyyy-mm-dd" that 
#'   can be converted to a \code{Date} object representing the first day for
#'   which to request data. Default: \code{as.character(Sys.Date() - 90L)}
#' @param type one of "single" (the default), "daily", "monthly"
#' @param include_raw_time if \code{TRUE} the original time column and the 
#'   column with the corrected winter time are included in the output. The
#'   default is \code{FALSE}.
#' @return data frame read from the CSV file that the download provides. 
#'   IMPORTANT: It is not yet clear how to interpret the timestamp, see example
#' @importFrom httr POST content
#' @importFrom utils read.table
#' @export
#' @examples 
#' # Get a list of available water quality stations and variables
#' stations <- kwb.read::get_wasserportal_stations()
#' variables <- kwb.read::get_wasserportal_variables()
#' 
#' # Read the timeseries (multiple variables for one station)
#' water_quality <- kwb.read::read_wasserportal(
#'   station = stations$MPS_Charlottenburg,
#'   variables = c(variables["Sauerstoffgehalt"], variables["Leitfaehigkeit"]),
#'   from_date = as.character(Sys.Date() - 30L), include_raw_time = TRUE
#' )
#' 
#' # Look at the first few records
#' head(water_quality)
#' 
#' # Check the metadata
#' kwb.utils::getAttribute(water_quality, "metadata")
#' 
#' # Set missing values to NA
#' water_quality[water_quality == -777] <- NA
#' 
#' # Look at the first few records again
#' head(water_quality)
#' 
#' ### How was the original timestamp interpreted?
#' 
#' # Determine the days at which summer time starts and ends, respectively
#' switches <- kwb.datetime::date_range_CEST(2019)
#' 
#' # Reformat to dd.mm.yyyy
#' switches <- kwb.datetime::reformatTimestamp(switches, "%Y-%m-%d", "%d.%m.%Y")
#' 
#' # Define a pattern to look for timestamps "around" the switches
#' pattern <- paste(switches, "0[1-4]", collapse = "|")
#' 
#' # Look at the data for these timestamps
#' water_quality[grepl(pattern, water_quality$timestamp_raw), ]
#' 
#' # The original timestamps (timestamps_raw) are not all plausible, e.g. 
#' # "31.03.2019 03:00" appears twice! See the Details in ?read_wasserportal()
#' # how this is treated.
read_wasserportal <- function(
  station, variables = get_wasserportal_variables(station), 
  from_date = as.character(Sys.Date() - 90L), type = "single", 
  include_raw_time = FALSE
)
{
  #kwb.utils::assignPackageObjects("kwb.read")
  #station=get_wasserportal_stations(type = "flow")$Tiefwerder
  #variables = get_wasserportal_variables(station);from_date = "2019-01-01";include_raw_time = FALSE
  
  variable_ids <- get_wasserportal_variables()
  station_ids <- get_wasserportal_stations(type = NULL)
  
  stopifnot(all(station %in% station_ids))
  stopifnot(all(variables %in% variable_ids))
  
  names(variables) <- names(variable_ids)[match(variables, variable_ids)]
  
  handle <- httr::handle_find(get_wasserportal_url(0, 0))
  
  dfs <- lapply(
    X = variables, 
    FUN = read_wasserportal_raw, 
    station = station, 
    from_date = from_date, 
    type = type,
    include_raw_time = include_raw_time,
    handle = handle
  )
  
  # Remove elements of class "response" that are returned in case of an error
  failed <- sapply(dfs, function(df) {
    inherits(df, "response") || length(df) == 0
  })
  
  if (any(failed)) {
    kwb.utils::catAndRun(
      sprintf("Removing %d elements that are empty or failed", sum(failed)), 
      expr = {
        failures <- dfs[failed]
        dfs <- dfs[! failed]
      }
    )
  } 
  
  if (length(dfs) == 0) {
    message("No remaining data frames. Returning NULL.")
    return(NULL)
  }
  
  result <- if (type == "single") {
    
    merge_raw_results_single(dfs, variables, include_raw_time)
      
  } else if (type == "daily") {

    merge_raw_results_daily(dfs)
    
  } else if (type == "monthly") {
    
    merge_raw_results_monthly(dfs)
    
  } else {
    
    stop("type must be one of 'single', 'daily', 'monthly'")
  }
  
  metadata <- lapply(dfs, kwb.utils::getAttribute, "metadata")
  
  structure(
    result, 
    metadata = metadata, 
    failures = if (any(failed)) failures
  )
}

# merge_raw_results_single -----------------------------------------------------
merge_raw_results_single <- function(dfs, variables, include_raw_time)
{
  date_vectors <- lapply(dfs, kwb.utils::selectColumns, "LocalDateTime")
  
  if (length(variables) > 1 && ! kwb.utils::allAreIdentical(date_vectors)) {
    message("Not all requests return the same timestamp column:")
    kwb.utils::printIf(TRUE, lengths(date_vectors))
  }
  
  keys <- c(
    if (include_raw_time) c("timestamp_raw", "timestamp_corr"), 
    "LocalDateTime"
  )
  
  backbones <- lapply(dfs, kwb.utils::selectColumns, keys, drop = FALSE)
  
  backbone <- unique(do.call(rbind, backbones))
  
  backbone <- backbone[order(backbone$LocalDateTime), , drop = FALSE]
  
  backbone$row <- seq_len(nrow(backbone))
  
  data_frames <- c(list(base = backbone), dfs)
  
  result <- kwb.utils::mergeAll(
    data_frames, by = keys, all.x = TRUE, dbg = FALSE
  )
  
  result <- kwb.utils::removeColumns(result[order(result$row), ], "row.base")
  
  names(result) <- gsub("Einzelwert\\.", "", names(result))
  
  utc_offset <- kwb.datetime::utcOffset(
    LocalDateTime = format(result$LocalDateTime), 
    DateTimeUTC = format(result$LocalDateTime, tz = "UTC")
  )
  
  kwb.utils::insertColumns(
    result, after = "LocalDateTime", UTCOffset = utc_offset
  )
}

# merge_raw_results_daily ------------------------------------------------------
merge_raw_results_daily <- function(dfs)
{
  warning_not_implemented("merge_raw_results_daily()")
  dfs
}

# merge_raw_results_monthly ----------------------------------------------------
merge_raw_results_monthly <- function(dfs)
{
  warning_not_implemented("merge_raw_results_monthly()")
  dfs
}

# warning_not_implemented ------------------------------------------------------
warning_not_implemented <- function(x)
{
  warning(x, " is not yet implemented. Returning raw data")
}
