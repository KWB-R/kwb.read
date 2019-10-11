# read_wasserportal_raw --------------------------------------------------------

#' Download and Read Data from wasserportal.berlin.de
#' 
#' @param station station number, one of \describe{
#'   \item{601}{MPS Berlin-Spandauer-Schifffahrtskanal}
#'   \item{151}{MPS Caprivibruecke}
#'   \item{153}{MPS Charlottenburg}
#'   \item{509}{MPS Landwehrkanal}
#'   \item{504}{MPS Neukoellner Schifffahrtskanal}
#'   \item{414}{MPS Teltowkanal}
#'   \item{141}{MS Muehlendammschleuse}
#'   \item{111}{MS Rahnsdorf}
#'   \item{211}{MS Schmoeckwitz}
#'   \item{161}{MS Sophienwerder}
#'   \item{421}{MS Teltow-Werft}
#' }
#' @param from_date \code{Date} object (or string in format "yyyy-mm-dd" that 
#'   can be converted to a \code{Date} object representing the first day for
#'   which to request data
#' @return data frame read from the CSV file that the download provides. 
#'   IMPORTANT: It is not yet clear how to interpret the timestamp, see example
#' @importFrom httr POST content
#' @importFrom utils read.table
#' @export
#' @examples 
#' # Get a list of available water quality stations
#' stations <- kwb.read::get_wasserportal_stations()
#' temperature_raw <- kwb.read::read_wasserportal_raw(stations$MPS_Charlottenburg)
#' 
#' # Look at the first few records
#' head(temperature_raw)
#' 
#' # Check the metadata
#' kwb.utils::getAttribute(temperature_raw, "metadata")
#' 
#' # Set missing values to NA
#' temperature_raw$Einzelwert[temperature_raw$Einzelwert == -777] <- NA
#' 
#' # Look at the first few records
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
read_wasserportal_raw <- function(station, from_date = "2019-01-01")
{
  if (! inherits(from_date, "Date")) {
    
    from_date <- try(as.Date(from_date))
    
    if (inherits(from_date, "try-error")) {
      stop(call. = FALSE, "from_date cannot be converted to a Date object!")
    }
  }
  
  # Helper function to read CSV
  read <- function(text, ...) utils::read.table(
    text = text, sep = ";", dec = ",", stringsAsFactors = FALSE, ...
  )
  
  url_base <- "https://wasserportal.berlin.de/station.php?anzeige=td"
  
  url <- sprintf("%s&sstation=%s", url_base, station)
  
  # Format the start date
  sdatum <- format(from_date, format = "%d.%m.%Y")
  
  # Compose the body of the request
  body <- list(sreihe = "w", smode = "c", sdatum = sdatum)
  
  # Post the request to the web server
  response <- httr::POST(url, body = body)
  
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
