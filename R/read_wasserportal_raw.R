# read_wasserportal ------------------------------------------------------------

#' Download and Read Data from wasserportal.berlin.de
#' 
#' @param station station number, one of \itemize{
#'   \item{601: MPS Berlin-Spandauer-Schifffahrtskanal}
#'   \item{151: MPS Caprivibruecke}
#'   \item{153: MPS Charlottenburg}
#'   \item{509: MPS Landwehrkanal}
#'   \item{504: MPS Neukoellner Schifffahrtskanal}
#'   \item{414: MPS Teltowkanal}
#'   \item{141: MS Muehlendammschleuse}
#'   \item{111: MS Rahnsdorf}
#'   \item{211: MS Schmoeckwitz}
#'   \item{161: MS Sophienwerder}
#'   \item{421: MS Teltow-Werft}
#' }
#' 
#' @param variables vector of variable identifiers, each being one of \itemize{
#'   \item{"t": Wassertemperatur}
#'   \item{"l": Leitfaehigkeit}
#'   \item{"p": pH_Wert}
#'   \item{"o": Sauerstoffgehalt}
#'   \item{"s": Sauerstoffsaettigung}
#' }
#' 
#' @param from_date \code{Date} object (or string in format "yyyy-mm-dd" that 
#'   can be converted to a \code{Date} object representing the first day for
#'   which to request data
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
#' # Read the raw timeseries
#' temperature_raw <- kwb.read::read_wasserportal(
#'   station = stations$MPS_Charlottenburg,
#'   variables = c(variables["Sauerstoffgehalt"], variables["Leitfaehigkeit"]),
#'   from_date = "2019-03-01"
#' )
#' 
#' # Look at the first few records
#' head(temperature_raw)
#' 
#' # Check the metadata
#' kwb.utils::getAttribute(temperature_raw, "metadata")
#' 
#' # Set missing values to NA
#' temperature_raw[temperature_raw == -777] <- NA
#' 
#' # Look at the first few records again
#' head(temperature_raw)
#' 
#' # How to interpret the timestamp?
#' # Determine the days at which summer time starts and ends, respectivel
#' switches <- kwb.datetime::date_range_CEST(2019)
#' 
#' # Reformat to dd.mm.yyyy
#' switches <- kwb.datetime::reformatTimestamp(switches, "%Y-%m-%d", "%d.%m.%Y")
#' 
#' # Define a pattern to look for timestamps "around" the switches
#' pattern <- paste(switches, "0[1-4]", collapse = "|")
#' 
#' # Look at the data for these timestamps
#' temperature_raw[grepl(pattern, temperature_raw$Datum), ]
#' 
#' # The timestamps are not plausible, e.g. "31.03.2019 03:00" appears twice!
read_wasserportal <- function(
  station, variables = get_wasserportal_variables(station), 
  from_date = "2019-01-01"
)
{
  #kwb.utils::assignPackageObjects("kwb.read")
  #variables = get_wasserportal_variables(station);from_date = "2019-01-01"
  
  variable_ids <- get_wasserportal_variables()
  station_ids <- get_wasserportal_stations(type = NULL)
  
  stopifnot(all(station %in% station_ids))
  stopifnot(all(variables %in% variable_ids))

  names(variables) <- names(variable_ids)[match(variables, variable_ids)]
  
  dfs <- lapply(variables, function(variable) {
    #variable <- variables[1]
    df <- read_wasserportal_raw(station, variable, from_date)
    stopifnot(! any(duplicated(df$LocalDateTime)))
    df
  })
  
  date_vectors <- lapply(dfs, kwb.utils::selectColumns, "LocalDateTime")
  
  if (length(variables) > 1 && ! kwb.utils::allAreIdentical(date_vectors)) {
    message("Not all requests return the same timestamp column:")
    kwb.utils::printIf(TRUE, lengths(date_vectors))
  }
  
  keys <- c("timestamp_raw", "timestamp_corr", "LocalDateTime")
  
  backbones <- lapply(dfs, kwb.utils::selectColumns, keys)
  
  backbone <- unique(do.call(rbind, backbones))
  
  backbone <- backbone[order(backbone$LocalDateTime), ]
  
  backbone$row <- seq_len(nrow(backbone))
  
  data_frames <- c(list(base = backbone), dfs)
  
  result <- kwb.utils::mergeAll(data_frames, by = keys, all.x = TRUE)
  
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
read_wasserportal_raw <- function(station, variable, from_date)
{
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
  
  data$LocalDateTime <- kwb.datetime::textToEuropeBerlinPosix(
    data$timestamp_corr, format = "%d.%m.%Y %H:%M", switches = FALSE
  )
  
  keys <- c("timestamp_raw", "timestamp_corr", "LocalDateTime")
  
  data <- kwb.utils::moveColumnsToFront(data, keys)
  
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
  
  stopifnot(all(grepl(" 03", timestamps[first_indices])))
  
  timestamps_old <- timestamps
  
  timestamps[first_indices] <- gsub(" 03", " 02", timestamps[first_indices])
  
  indices <- sort(unlist(index_pairs))
  
  kwb.utils::printIf(dbg, caption = "After timestamp repair", data.frame(
    row = indices, 
    old = timestamps_old[indices], 
    new = timestamps[indices]
  ))
  
  stopifnot(! any(duplicated(timestamps)))
  
  timestamps
}
