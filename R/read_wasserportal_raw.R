# read_wasserportal_raw --------------------------------------------------------
# @param type one of "single", "daily", "monthly"
read_wasserportal_raw <- function(
  variable, station, from_date, type = "single", include_raw_time = FALSE, 
  handle = NULL
)
{
  #variable <- variables[1]
  from_date <- assert_date(from_date)
  
  stopifnot(length(station) == 1)
  station_ids <- get_wasserportal_stations(type = NULL)
  stopifnot(station %in% station_ids)
  
  stopifnot(length(variable) == 1)
  variable_ids <- get_wasserportal_variables(station)
  stopifnot(variable %in% variable_ids)
  
  sreihe <- kwb.utils::selectElements(elements = type, list(
    single = "w", daily = "m", monthly = "j"
  ))
  
  # Helper function to read CSV
  read <- function(text, ...) {
    
    result <- try(silent = TRUE, utils::read.table(
      text = text, sep = ";", dec = ",", stringsAsFactors = FALSE, ...
    ))
    
    if (! inherits(result, "try-error")) {
      result
    }
  }

  progress <- get_wasserportal_text(station, variable, station_ids, variable_ids)
  url <- get_wasserportal_url(station, variable)
  
  # Format the start date
  sdatum <- format(from_date, format = "%d.%m.%Y")
  
  # Compose the body of the request
  body <- list(sreihe = sreihe, smode = "c", sdatum = sdatum)
  
  # Post the request to the web server
  response <- kwb.utils::catAndRun(
    progress, 
    httr::POST(url, body = body, handle = handle)
  )
  
  if (httr::http_error(response)) {
    message("POST request failed. Returning the response object.")
    return(response)
  }
  
  # Read the response of the web server as text
  text <- httr::content(response, as = "text", encoding = "Latin1")
  
  # Split the text into separate lines
  textlines <- strsplit(text, "\n")[[1]]

  # Split the header row into fields
  header_fields <- as.character(read(textlines[1]))
  
  # Return empty list with metadata if no data rows are available
  if (length(textlines) == 1L) {
    return(add_wasserportal_metadata(list(), header_fields))
  }

  # Read the data rows
  data <- read(text, header = FALSE, skip = 1)
  
  # Get the numbers of the data columns 
  if (type != "monthly") {
    stopifnot(ncol(data) == 2L)
  }
  
  # Name the data columns as given in the first columns of the header row
  names(data) <- header_fields[seq_len(ncol(data))]

  if (type == "single") {
    data <- clean_timestamp_columns(data, include_raw_time)
  }  
  
  # Return the data frame with the additional fields of the header row as
  # meta information in attribute "metadata"
  add_wasserportal_metadata(data, header_fields)
}

# clean_timestamp_columns ------------------------------------------------------
clean_timestamp_columns <- function(data, include_raw_time)
{
  raw_timestamps <- kwb.utils::selectColumns(data, "Datum")
  
  data <- kwb.utils::renameColumns(data, list(Datum = "timestamp_raw"))
  
  data$timestamp_corr <- repair_wasserportal_timestamps(
    timestamps = raw_timestamps
  )
  
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
  
  remove_timestep_outliers(data, data$LocalDateTime, 60 * 15)
}

# add_wasserportal_metadata ----------------------------------------------------
add_wasserportal_metadata <- function(x, header_fields)
{
  structure(x, metadata = header_fields[-(1:2)])
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

# get_wasserportal_url ---------------------------------------------------------
get_wasserportal_url <- function(station, variable)
{
  url_base <- "https://wasserportal.berlin.de/station.php"
  
  sprintf("%s?sstation=%s&anzeige=%sd", url_base, station, variable)
}

# repair_wasserportal_timestamps -----------------------------------------------
repair_wasserportal_timestamps <- function(timestamps, dbg = FALSE)
{
  duplicates <- timestamps[duplicated(timestamps)]
  
  index_pairs <- lapply(duplicates, function(x) which(timestamps == x))
  
  if (length(index_pairs) == 0L) {
    return(timestamps)
  }
  
  stopifnot(all(lengths(index_pairs) == 2L))
  
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
