# read_wasserportal_raw --------------------------------------------------------

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
#' temperature_raw <- kwb.read::read_wasserportal_raw(
#'   station = stations$MPS_Charlottenburg, 
#'   variables = c(variables$Sauerstoffgehalt, variables$Leitfaehigkeit), 
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
read_wasserportal_raw <- function(
  station, 
  variables = unlist(get_wasserportal_variables())[1:4], 
  from_date = "2019-01-01"
)
{
  if (! inherits(from_date, "Date")) {
    
    from_date <- try(as.Date(from_date))
    
    if (inherits(from_date, "try-error")) {
      stop(call. = FALSE, "from_date cannot be converted to a Date object!")
    }
  }
  
  variable_ids <- get_wasserportal_variables()
  station_ids <- get_wasserportal_stations()
  
  stopifnot(all(station %in% station_ids))
  stopifnot(all(variables %in% variable_ids))

  if (length(variables) > 1) {
  
    dfs <- lapply(
      variables, read_wasserportal_raw, station = station, from_date = from_date
    )

    date_vectors <- lapply(dfs, kwb.utils::selectColumns, "Datum")
    
    if (! kwb.utils::allAreIdentical(date_vectors)) {
      
      kwb.utils::printIf(TRUE, lapply(date_vectors, range))
      kwb.utils::printIf(TRUE, lengths(date_vectors))
      
      stop("Not all requests return the same timestamp column (see above)!")
    }
    
    result <- kwb.utils::noFactorDataFrame(Datum = dfs[[1]]$Datum)
    
    value_data <- do.call(data.frame, lapply(dfs, "[[", 2))
    
    names(value_data) <- names(variable_ids)[match(variables, variable_ids)]
    
    metadata <- lapply(dfs, kwb.utils::getAttribute, "metadata")
    
    names(metadata) <- names(value_data)
      
    return(structure(cbind(result, value_data), metadata = metadata))
  }
  
  # Helper function to read CSV
  read <- function(text, ...) utils::read.table(
    text = text, sep = ";", dec = ",", stringsAsFactors = FALSE, ...
  )
  
  url_base <- "https://wasserportal.berlin.de/station.php"
  
  url <- sprintf("%s?anzeige=%sd&sstation=%s", url_base, variables, station)
  
  # Format the start date
  sdatum <- format(from_date, format = "%d.%m.%Y")
  
  # Compose the body of the request
  body <- list(sreihe = "w", smode = "c", sdatum = sdatum)
  
  # Post the request to the web server
  response <- kwb.utils::catAndRun(
    sprintf(
      "Reading '%s' for station %s (%s)", 
      names(variable_ids)[match(variables, unlist(variable_ids))], 
      station, 
      names(station_ids)[match(station, unlist(station_ids))]
    ), 
    httr::POST(url, body = body)
  )
  
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
  
  # Return the data frame with the additional fields of the header row as
  # meta information in attribute "metadata"
  structure(data, metadata = header_fields[- first_cols])
}

# get_wasserportal_stations ----------------------------------------------------

#' Get Names and IDs of the Stations of wasserportal.berlin.de
#' 
#' @export
get_wasserportal_stations <- function()
{
  list(
    MPS_Berlin_Spandauer_Schifffahrtskanal = 601,
    MPS_Caprivibruecke = 151,
    MPS_Charlottenburg = 153,
    MPS_Landwehrkanal = 509,
    MPS_Neukoellner_Schifffahrtskanal = 504,
    MPS_Teltowkanal = 504,
    MS_Muehlendammschleuse = 141,
    MS_Rahnsdorf = 111,
    MS_Schmoeckwitz = 211,
    MS_Sophienwerder = 161,
    MS_Teltow_Werft = 421
  )
}

# get_wasserportal_variables ---------------------------------------------------

#' Get Names and IDs of the Variables of wasserportal.berlin.de
#' 
#' @export
get_wasserportal_variables <- function()
{
  list(
    Wassertemperatur = "t",
    Leitfaehigkeit = "l",
    pH_Wert = "p",
    Sauerstoffgehalt = "o",
    Sauerstoffsaettigung = "s"
  )
}
