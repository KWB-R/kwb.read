# read_BWB_rain_correction -----------------------------------------------------

#' Read Correction Data
#' 
#' Read BWB's rain correction data (different formats supported)
#' 
#' @param files paths to input files
#' @param type file type, must be one of the type strings returned by 
#'   \code{kwb.read:::BWB_rain_correction_types}
#' @param \dots arguments passed to the read-function corresponding to the
#'   given \code{type}
#' 
#' @export
#' 
read_BWB_rain_correction <- function(
  files, type = (BWB_rain_correction_types())[1], ...
)
{
  types <- kwb.utils::toNamedList(BWB_rain_correction_types())

  if (type == types$long) {

    read_BWB_rain_correction_long(files, ...)

  } else if (type == types$wide.day) {

    read_BWB_rain_correction_wide_day(files, ...)

  } else if (type == types$wide.gauge) {

    read_BWB_rain_correction_wide_gauge(files, ...)

  } else if (type == types$wide.gauge.from.pdf) {
    
    read_BWB_rain_correction_wide_gauge_from_pdf(files, ...)
    
  } else if (type == types$wide.gauge.group) {
    
    read_BWB_rain_correction_wide_gauge_group(file = files, ...)
    
  } else {

    stop("type must be one of ", kwb.utils::stringList(as.character(types)))
  }
}

# BWB_rain_correction_types ----------------------------------------------------
BWB_rain_correction_types <- function()
{
  c(
    "long", "wide.day", "wide.gauge", "wide.gauge.from.pdf", "wide.gauge.group"
  )
}

# read_BWB_rain_correction_long ------------------------------------------------

#' Read BWB Rain Correction Data
#' 
#' Read BWB Rain Correction Data in "long" format. The data is provided by BWB
#' in the form of an Excel file. Save the file to CSV and run this function on
#' that CSV file.
#' 
#' @param files full path to CSV file containing rain height correction data in
#'   "long" format
#' @param sep column separator in \code{files}
#' @param country one of "de" (German) or "en" (English) according to the format
#'   numeric strings are given in
#' @param date.format format used to convert the date string into a date object
#' @param locale locale string, passed to
#'   \code{\link{read_BWB_rain_correction_long}}
#' @param wide if \code{TRUE} (default) the data will be returned in "wide"
#'   format, i.e. with the date in the first column and each further column 
#'   representing a rain gauge
#' @param dbg logical. If \code{TRUE}, debug messages are shown.
#' 
#' @return data frame either in "wide" format (with the date in the first column
#'   and the rain heights in the following columns named according to the
#'   "object ids" of the rain gauges) or in "long" format (with columns
#'   \code{Date}, \code{ObjNr} \code{Pumpwerk} \code{Niederschlag}). The mapping
#'   between object ids and gauge names is returned in the attribute "gauges".
#' 
#' @examples 
#' \dontrun{
#' # Provide the path to an example file in this package
#' file <- "Niederschlag_example.csv"
#' file <- system.file("extdata", file, package = "kwb.read")
#'   
#' # Read the CSV file into a data frame in "long" format
#' correction.wide <- read_BWB_rain_correction_long(file)
#'   
#' # Read the CSV file into a data frame in "wide" format (gauges in columns)
#' correction.long <- read_BWB_rain_correction_long(file, wide = FALSE)
#'   
#' # Get the mapping between object IDs and gauge names from attribute "gauges"
#' kwb.utils::getAttribute(correction.wide, "gauges")
#' }
#' @export
#' 
read_BWB_rain_correction_long <- function(
  files, sep = ";", country = c("de", "en")[1], date.format = "%A %d. %B %Y",
  locale = .localeString(country), wide = TRUE, dbg = TRUE
)
{
  #sep=",";country="de";wide=TRUE;dbg=TRUE
  
  # files must be character
  stopifnot(is.character(files))
  
  # If more than one file is given, call this function for each file, merge
  # the results and return
  if (length(files) > 1) {
    
    dataList <- lapply(
      files, read_BWB_rain_correction_long, sep, country, date.format, locale, 
      wide, dbg
    )
    
    return (structure(
      data = kwb.utils::safeRowBindAll(dataList), 
      gauges = .mergeGaugeLists(dataList), 
      means = .mergeMeansLists(dataList), 
      files = files
    ))
  }
  
  .stopOnTooFewFiles(files)
  
  message("Reading ", kwb.utils::hsQuoteChr(basename(files[1])), "...")
  
  # Read the raw data from the CSV file
  data <- utils::read.table(
    kwb.utils::safePath(files), sep = sep, stringsAsFactors = FALSE,
    as.is = TRUE
  )
  
  # Delete empty columns ("Dummy")
  FUN <- function(x) all(kwb.utils::isNaOrEmpty(x))
  data <- kwb.utils::removeEmptyColumns(data, FUN = FUN, dbg = FALSE)
  
  # At least four columns expected
  #kwb.read:::.stopOnTooFewColumns(data, 4)
  .stopOnTooFewColumns(data, 4)
  
  names(data) <- c("ObjNr", "Pumpwerk", "Niederschlag", "Date")
  
  # Find the rows containing the date strings
  blockstarts <- which(data$ObjNr == "Berliner")
  
  if (! length(blockstarts)) {
    
    stop(
      "No row contains the keyword 'Berliner' in the first column! ",
      .text.wrong.format()
    )
  }
  
  # Extract the date strings
  datestrings <- data$Date[blockstarts]
  
  if (all(kwb.utils::isNaOrEmpty(datestrings))) {
    
    stop(
      "No dates found in the fourth non-empty column. ",
      .text.wrong.format()
    )
  }
  
  # If the datestrings look like numerics, treat the number as the day number
  # since 1899-12-30 (as it is done in Excel)
  datestrings <- .convertToDateStrings(
    datestrings, format = date.format, locale = locale
  )
  
  data$Date[blockstarts] <- datestrings
  data$Date[data$Date == ""] <- NA
  
  # Fill up the empty fields in the date column by copying the last value down
  data$Date <- kwb.utils::naToLastNonNa(data$Date)
  
  # Extract the mean values
  means <- data[data$Pumpwerk == "Mittelwert:", c(4, 3)]
  means <- kwb.utils::resetRowNames(means[order(means[, 1]), ])
  means[, 2] <- kwb.utils::hsChrToNum(means[, 2], country)
  
  # Filter for the data rows (containing a valid object id in the first column)
  data <- data[grepl("\\d\\d\\.\\d\\d", data$ObjNr), ]
  
  .stopOnDuplicateGauges(data$ObjNr, groups = data$Date, files)
  
  stats::aggregate(data$ObjNr, by = list(data$Date), function(x) max(table(x)))
  
  # Convert the rain height column to numeric
  data$Niederschlag <- kwb.utils::hsChrToNum(data$Niederschlag, country)
  
  # Move date column to the left
  data <- kwb.utils::moveColumnsToFront(data, "Date")
  
  # Get the assignment between object id and gauge
  gauges <- unique(kwb.utils::selectColumns(data, c("ObjNr", "Pumpwerk")))
  gauges <- kwb.utils::resetRowNames(gauges[order(gauges[, 1]), ])
  
  # Convert to wide format if required
  if (wide) {
    
    data <- stats::reshape(
      data, direction = "wide", timevar = "ObjNr", idvar = "Date",
      drop = "Pumpwerk"
    )
    
    names(data) <- gsub("^Niederschlag\\.", "", names(data))
    
    attr(data, "reshapeWide") <- NULL
  }
  
  data <- kwb.utils::resetRowNames(data)
  
  structure(data, gauges = gauges, means = means, files = files)
}

# readBwbRainCorrection --------------------------------------------------------

#' Read BWB Rain Correction Data
#' 
#' read BWB rain correction data from Excel file or CSV file
#' 
#' @param file full path to Excel file or CSV file
#' @param \dots arguments given to readBwbCorrFromExcel or readBwbCorrFromCSV
#' @param zerolines.rm logical. If \code{TRUE}, rows in which the sum of 
#'   absolute values is zero, are removed
#' @param dbg logical. If \code{TRUE}, debug messages are shown.
#' 
#' @export
#' 
readBwbRainCorrection <- function(file, ..., zerolines.rm = TRUE, dbg = FALSE)
{
  isCSV <- kwb.utils::stringEndsWith(file, ".csv")

  what <- ifelse(isCSV, "readBwbCorrFromCsv", "readBwbCorrFromExcel")

  rainCorrection <- do.call(what, args = list(file, ...))

  ## if there are single NA-values, replace them with 0.0 and give a warning
  if (any(is.na(rainCorrection))) {

    ## generate the warning message
    msg <- sprintf("NA values have been set to 0.0 in %s:", file)

    for (i in seq_len(nrow(rainCorrection))) {

      if (any(is.na(rainCorrection[i, ]))) {
        
        msg <- c(msg, paste(collapse = "\n", utils::capture.output(
          print(rainCorrection[i, ])
        )))
      }
    }

    ## give the warning
    warning(paste(msg, collapse = "\n"))

    ## replace NA with 0.0
    rainCorrection[is.na(rainCorrection)] <- 0.0
  }

  ## Calculate the sum for each row
  mat <- as.matrix(rainCorrection[, -1])
  rainCorrection$sum <- rowSums(mat)
  rainCorrection$abssum <- rowSums(abs(mat))

  ## If desired, remove rows in which the sum of absolute values is zero
  if (isTRUE(zerolines.rm)) {
    
    rainCorrection <- rainCorrection[rainCorrection$abssum != 0, ]
  }

  rainCorrection
}

# readBwbCorrFromExcel ---------------------------------------------------------

#' Read BWB Rain Correction Data from Excel File
#' 
#' @param file full path to Excel file
#' @param tblCorr name of sheet containing correction data. Default: 
#'   "Bericht 1$"
#' @param tblGaug name of sheet containing gauge Names. Default: "gaugeNames$"
#' @param dbg logical. If \code{TRUE}, debug messages are shown.
#' 
readBwbCorrFromExcel <- function(
  file, tblCorr = "Bericht 1$", tblGaug = "gaugeNames", dbg = TRUE
)
{
  ## Read gauge names
  gaugeNames <- kwb.db::hsGetTable(file, tblGaug, stringsAsFactors = FALSE)[1, ]

  ## Read rain correction table
  rainCorrection <- kwb.db::hsGetTable(file, tblCorr, stringsAsFactors = FALSE)

  enumeration <- paste(kwb.utils::hsQuoteChr(gaugeNames), collapse = ", ")

  kwb.utils::catIf(dbg, "Rain gauge names: ", enumeration, "\n")

  ## Delete NA rows and NAcolumns
  rainCorrection <- kwb.base::hsDelNaRowsOrCols(rainCorrection, rows = TRUE)
  rainCorrection <- kwb.base::hsDelNaRowsOrCols(rainCorrection, rows = FALSE)

  ## Assign gauge names to columns
  names(rainCorrection) <- gaugeNamesShort(gaugeNames)
  names(rainCorrection)[1] <- "tDate_BWB"

  ## Convert first column to Date
  rainCorrection$tDate_BWB = as.Date(kwb.datetime::hsToPosix(
    kwb.utils::selectColumns(rainCorrection, "tDate_BWB")
  ))

  rainCorrection
}

# readBwbCorrFromCsv -----------------------------------------------------------

#' Read BWB Rain Correction Data from CSV File
#' 
#' Read BWB rain correction data from a CSV file that has been created by saving
#' the corresponding Excel file as CSV
#'
#' @param file path to CSV file
#' @param sep column separator
#' @param country contry code specifying decimal and thousands separators, one
#' @param format date format, passed to \code{\link{as.Date}}
#'  
readBwbCorrFromCsv <- function(
  file, sep = ",", country = "de", format = "%d.%m.%Y"
)
{
  ## Read gauge names
  captionLine <- readLines(file, 3)[3]

  gaugeNames <- strsplit(captionLine, ",")[[1]]

  ## Read rain correction table
  rainCorrection <- utils::read.table(
    file, skip = 3, sep = sep, stringsAsFactors = FALSE,
    colClasses = rep("character", length(gaugeNames))
  )

  ## Assign gauge names to columns
  names(rainCorrection) <- gaugeNamesShort(gaugeNames)

  # Remove first (empty) column
  rainCorrection <- rainCorrection[, -1]

  # The first column is the date column
  names(rainCorrection)[1] <- "tDate_BWB"

  # Convert all columns except the first column to numeric
  for (i in seq_along(rainCorrection)[-1]) {
    
    rainCorrection[, i] <- kwb.utils::hsChrToNum(
      rainCorrection[, i], country = country
    )
  }

  # Convert the first column to Date
  rainCorrection$tDate_BWB = as.Date(rainCorrection[, 1], format = format)

  rainCorrection
}
