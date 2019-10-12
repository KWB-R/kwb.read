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
#'   which to request data
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
#'   from_date = "2019-03-01", include_raw_time = TRUE
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
  from_date = "2019-01-01", include_raw_time = FALSE
)
{
  #kwb.utils::assignPackageObjects("kwb.read")
  #variables = get_wasserportal_variables(station);from_date = "2019-01-01";include_raw_time = FALSE
  
  variable_ids <- get_wasserportal_variables()
  station_ids <- get_wasserportal_stations(type = NULL)
  
  stopifnot(all(station %in% station_ids))
  stopifnot(all(variables %in% variable_ids))

  names(variables) <- names(variable_ids)[match(variables, variable_ids)]
  
  dfs <- lapply(
    X = variables, 
    FUN = read_wasserportal_raw, 
    station = station, 
    from_date = from_date, 
    include_raw_time = include_raw_time
  )
  
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
  
  result <- kwb.utils::insertColumns(
    result, after = "LocalDateTime", UTCOffset = utc_offset
  )
  
  metadata <- lapply(dfs, kwb.utils::getAttribute, "metadata")

  return(structure(result, metadata = metadata))
}

# read_wasserportal_raw --------------------------------------------------------
read_wasserportal_raw <- function(
  variable, station, from_date, include_raw_time = FALSE
)
{
  #variable <- variables[2]
  from_date <- assert_date(from_date)
  
  stopifnot(length(station) == 1)
  station_ids <- get_wasserportal_stations(type = NULL)
  stopifnot(station %in% station_ids)
  
  stopifnot(length(variable) == 1)
  variable_ids <- get_wasserportal_variables(station)
  stopifnot(variable %in% variable_ids)
  
  # Helper function to read CSV
  read <- function(text, ...) utils::read.table(
    text = text, sep = ";", dec = ",", stringsAsFactors = FALSE, ...
  )
  
  progress <- get_wasserportal_text(station, variable, station_ids, variable_ids)
  url <- get_wasserportal_url(station, variable)
  
  # Format the start date
  sdatum <- format(from_date, format = "%d.%m.%Y")
  
  # Compose the body of the request
  body <- list(sreihe = "w", smode = "c", sdatum = sdatum)
  
  # Post the request to the web server
  response <- kwb.utils::catAndRun(progress, httr::POST(url, body = body))
  
  # Read the response of the web server as text
  text <- httr::content(response, as = "text", encoding = "Latin1")
  
  # Read the header row
  header_row <- strsplit(substr(text, 1, 1000), "\n")[[1]][1]
  
  # Split the header row into fields
  header_fields <- as.character(read(header_row))
  
  # Read the data rows
  data <- read(text, header = FALSE, skip = 1)
  
  # Get the numbers of the data columns 
  first_cols <- seq_len(ncol(data))
  
  # Name the data columns as given in the first columns of the header row
  names(data) <- header_fields[first_cols]
  
  raw_timestamps <- kwb.utils::selectColumns(data, "Datum")
  
  data <- kwb.utils::renameColumns(data, list(Datum = "timestamp_raw"))
  
  data$timestamp_corr <- repair_wasserportal_timestamps(raw_timestamps)

  data <- remove_remaining_duplicates(data)
  
  data$LocalDateTime <- kwb.datetime::textToEuropeBerlinPosix(
    data$timestamp_corr, 
    format = "%d.%m.%Y %H:%M", 
    switches = FALSE, 
    dbg = FALSE
  )

  stopifnot(! any(duplicated(data$LocalDateTime)))
  
  keys <- c("timestamp_raw", "timestamp_corr", "LocalDateTime")
  
  data <- kwb.utils::moveColumnsToFront(data, keys)

  if (! include_raw_time) {
    data <- kwb.utils::removeColumns(data, keys[1:2])
  }
  
  data <- remove_timestep_outliers(data, data$LocalDateTime, 60 * 15)
  
  # Return the data frame with the additional fields of the header row as
  # meta information in attribute "metadata"
  structure(data, metadata = header_fields[- first_cols])
}

# get_wasserportal_url ---------------------------------------------------------
get_wasserportal_url <- function(station, variable)
{
  url_base <- "https://wasserportal.berlin.de/station.php"
  
  sprintf("%s?sstation=%s&anzeige=%sd", url_base, station, variable)
}

# get_wasserportal_text --------------------------------------------------------
get_wasserportal_text <- function(station, variable, station_ids, variable_ids)
{
  sprintf(
    "Reading '%s' for station %s (%s)", 
    names(variable_ids)[match(variable, unlist(variable_ids))], 
    station, 
    names(station_ids)[match(station, unlist(station_ids))]
  )
}

# get_wasserportal_stations ----------------------------------------------------

#' Get Names and IDs of the Stations of wasserportal.berlin.de
#' 
#' @param type one of "quality", "level", "flow"
#' @export
get_wasserportal_stations <- function(type = "quality")
{
  if (! is.null(type)) {
    type <- match.arg(type, c("quality", "level", "flow"))  
  }
  
  file <- "stations_wasserportal.csv"
  
  stations <- readPackageFile(file, fileEncoding = "UTF-8")
  
  get <- kwb.utils::selectColumns
  
  stations$id <- as.character(get(stations, "id"))
  stations$name <- kwb.utils::hsSubstSpecChars(get(stations, "name"))
  
  is_available <- if (is.null(type)) {
    seq_len(nrow(stations))
  } else {
    nzchar(get(stations, type))
  }
  
  kwb.utils::toLookupList(data = get(stations, c("name", "id"))[is_available, ])
}

# get_wasserportal_variables ---------------------------------------------------

#' Get Names and IDs of the Variables of wasserportal.berlin.de
#' 
#' @param station station id. If given, only variables that are available for 
#'   the given station are returned.
#' @export
get_wasserportal_variables <- function(station = NULL)
{
  variables <- list(
    quality = c(
      Wassertemperatur = "t",
      Leitfaehigkeit = "l",
      pH_Wert = "p",
      Sauerstoffgehalt = "o",
      Sauerstoffsaettigung = "s"
    ),
    level = c(Wasserstand = "w"),
    flow = c(Durchfluss = "d")
  )
  
  types <- names(variables)
  
  if (! is.null(station)) {
    
    types <- types[sapply(types, function(type) {
      
      station %in% get_wasserportal_stations(type)
    })]
  }
  
  unlist(lapply(types, function(element) variables[[element]]))
}

# repair_wasserportal_timestamps -----------------------------------------------
repair_wasserportal_timestamps <- function(timestamps, dbg = FALSE)
{
  duplicates <- timestamps[duplicated(timestamps)]
  
  index_pairs <- lapply(duplicates, function(x) which(timestamps == x))
  
  stopifnot(all(lengths(index_pairs) == 2))
  
  first_indices <- sapply(index_pairs, kwb.utils::firstElement)
  
  if (dbg && ! all(is_expected <- grepl(" 03", timestamps[first_indices]))) {
    
    message(
      "There are unexpected duplicated timestamps: ", 
      kwb.utils::stringList(timestamps[first_indices][! is_expected])
    )
  }
  
  timestamps_old <- timestamps
  
  timestamps[first_indices] <- gsub(" 03", " 02", timestamps[first_indices])
  
  indices <- sort(unlist(index_pairs))
  
  kwb.utils::printIf(dbg, caption = "After timestamp repair", data.frame(
    row = indices, 
    old = timestamps_old[indices], 
    new = timestamps[indices]
  ))
  
  timestamps
}

# remove_remaining_duplicates --------------------------------------------------
remove_remaining_duplicates <- function(data)
{
  timestamps <- kwb.utils::selectColumns(data, "timestamp_corr")
  
  is_duplicate <- duplicated(timestamps)
  
  if (any(is_duplicate)) {
    
    message("Removing rows with unexpected duplicated timestamps:")
    print(data[is_duplicate, ])
  }

  data[! is_duplicate, ]
}

# remove_timestep_outliers -----------------------------------------------------
remove_timestep_outliers <- function(data, timestamps, timestep = 15 * 60)
{
  stopifnot(inherits(timestamps, "POSIXct"))
  stopifnot(nrow(data) == length(timestamps))
  
  is_outlier <- as.numeric(timestamps) %% timestep != 0

  if (! any(is_outlier)) {
    return(data)
  }
  
  message("Removing rows with 'non-timestep-multiple' timestamps:")
  print(data[is_outlier, ])
  
  data[! is_outlier, ]
}
