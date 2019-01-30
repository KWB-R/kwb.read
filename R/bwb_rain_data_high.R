# readAllBwbCorrections --------------------------------------------------------

#' Read and Merge BWB Correction Files
#' 
#' @param dir.in full path to input directory where to look for CSV files to
#'   import
#' @param args.general list of general argument settings to be used when calling
#'   \code{\link{read_BWB_rain_correction}}. For available argument names, see 
#'   the help pages of the sub-functions 
#'   \code{\link{read_BWB_rain_correction_long}}, 
#'   \code{\link{read_BWB_rain_correction_wide_day}}, 
#'   \code{\link{read_BWB_rain_correction_wide_gauge}}.
#' @param args list of argument settings for each file. The elements in the list
#'   are expected to be the names of files in \code{dir.in}.
#' @param dbg if \code{TRUE} (default), debug messages are shown
#' 
#' @export
#' 
readAllBwbCorrections <- function(dir.in, args.general, args, dbg = TRUE)
{
  # Read all correction files with argument settings as defined in args
  files <- names(args)

  correctionsList <- lapply(files, function(file) {
    kwb.utils::callWith(
      read_BWB_rain_correction,
      args.general, # general arguments
      args[[file]], # specific arguments
      files = kwb.utils::safePath(file.path(dir.in, file))
    )
  })

  # Get the assignment between ID and gauge names
  gauges <- .mergeGaugeLists(correctionsList, strict = FALSE)
  
  # Replace German Umlaute in gauge names
  replacements = list("\\xe4" = "ae", "\\xf6" = "oe", "\\xfc" = "ue")
  gauges$Pumpwerk <- kwb.utils::multiSubstitute(gauges$Pumpwerk, replacements)
  
  # Replace column names where required
  correctionsList <- lapply(seq_along(correctionsList), function(i) {
    
    x <- correctionsList[[i]]
    
    # Rename the columns for gauges for which the IDs are known
    selected <- gauges$Pumpwerk %in% names(x)
    
    if (any(selected)) {
      
      cat(sprintf(
        "'%s': Renaming columns from gauge name to gauge ID...\n", files[i]
      ))
      
      x <- kwb.utils::renameColumns(x, renamings = kwb.utils::toLookupList(
        data = gauges[selected, 2:1]
      ))
      
      # Remove the columns for which the IDs are not known
      columns <- names(x)[! grepl("^(Date|\\d\\d\\.\\d\\d)$", names(x))]
      
      if (length(columns)) {
        
        x <- kwb.utils::removeColumns(x, columns)
        
        message(
          "The following columns were removed since the ID of the gauges ", 
          "are unknown:\n", kwb.utils::stringList(columns)
        )
      }
    }
    
    x
  })

  corrections <- kwb.utils::rbindAll(lapply(correctionsList, function(x) {

    x <- kwb.utils::hsMatrixToListForm(
      x, keyFields = "Date", colNamePar = "Gauge", colNameVal = "Value"
    )

    x[! is.na(x$Value), ]
  }))

  # Remove rows that are equal in all columns
  isDuplicated <- duplicated(corrections)

  if (any(isDuplicated)) {
    
    corrections <- corrections[! isDuplicated, ]
    
    kwb.utils::catIf(
      dbg, sum(isDuplicated), "duplicate rows in correction data removed.\n"
    )
  }

  # There should not be rows with duplicate Date and Gauge now!
  stopifnot(all(! duplicated(
    kwb.utils::selectColumns(corrections, c("Date", "Gauge"))
  )))
  
  # Convert back to wide format
  corrections <- stats::reshape(
    corrections, direction = "wide", timevar = "Gauge", idvar = "Date"
  )
  
  attr(corrections, "reshapeWide") <- NULL
  
  names(corrections) <- gsub("^Value\\.", "", names(corrections))

  # Order rows by date and value columns by name
  columns <- kwb.utils::moveToFront(sort(names(corrections)), "Date")

  corrections <- corrections[order(corrections$Date), columns]
  
  structure(kwb.utils::resetRowNames(corrections), gauges = gauges)
}

# readAllBwbSignals ------------------------------------------------------------

#' Read and Merge BWB Rain Data Files
#' 
#' Read and Merge BWB rain data files with \code{\link{readBwbRainFromCsv2}}
#' 
#' @param files.data character vector containing the full paths to the CSV files
#'   to be read
#' @param date.format format used to interpret the date string. See 
#'   \code{\link[base]{format.POSIXct}}
#' @param dbg if \code{TRUE} debug messages are shown
#'
#' @export
#' 
readAllBwbSignals <- function(files.data, date.format = "%Y-%m-%d", dbg = TRUE)
{
  # Read all rain data files
  rainData <- readAllBwbCumulativeHeights(files.data, dbg = dbg)

  gaugeInfo <- kwb.utils::getAttribute(rainData, "gaugeInfo")

  rainDiffs <- .toDifferences(rainData, dbg = dbg)

  signals <- .toSignals(rainDiffs)

  signals <- .removeDuplicates(signals, columns.key = names(signals)[1:3])

  # Order rows by date and time and value columns by name
  row.order <- order(signals[, 1], signals[, 1])

  keys <- names(signals)[1:3]

  columns <- kwb.utils::moveToFront(sort(names(signals)), keys)

  structure(signals[row.order, columns], gaugeInfo = gaugeInfo)
}

# readAllBwbCumulativeHeights --------------------------------------------------

#' Read and Combine Rain Heights
#' 
#' Read rain height files with \code{\link{readBwbRainFromCsv2}}, combine them
#' by rows, remove duplicates and order by timestamp
#' 
#' @param files.data character vector containing the full paths to the CSV files
#'   to be read
#' @param \dots arguments passed to \code{\link{readBwbRainFromCsv2}}
#' @param dbg if \code{TRUE} debug messages are shown
#' 
#' @export
#' 
readAllBwbCumulativeHeights <- function(files.data, ..., dbg = TRUE)
{
  n.files <- length(files.data)

  dataList <- lapply(seq_len(n.files), function(i) {
    
    kwb.utils::catIf(dbg, sprintf("[%i/%i] ", i, n.files))
    
    result <- try(kwb.read::readBwbRainFromCsv2(files.data[i], ...))
    
    if (! inherits(result, "try-error")) {
      
      result
    }
  })

  kwb.utils::catIf(dbg, "Removing possible NULL entries... ")
  dataList <- kwb.utils::excludeNULL(dataList)
  kwb.utils::catIf(dbg, "ok.\n")

  kwb.utils::catIf(dbg, "Combining data frames by rows... ")
  rainData <- kwb.utils::safeRowBindAll(dataList)
  kwb.utils::catIf(dbg, "ok.\n")

  kwb.utils::catIf(dbg, "Removing duplicates... ")
  rainData <- .removeDuplicates(rainData, columns.key = "Time")
  kwb.utils::catIf(dbg, "ok.\n")

  timestamps <- kwb.utils::selectColumns(rainData, "Time")

  stopifnot(! any(duplicated(timestamps)))

  kwb.utils::catIf(dbg, "Ordering by timestamp... ")
  rainData <- rainData[order(timestamps), ]
  kwb.utils::catIf(dbg, "ok.\n")

  # Merge the tables containing information about the gauges
  gaugeInfo <- lapply(dataList, kwb.utils::getAttribute, "gaugeInfo")
  gaugeInfo <- unique(kwb.utils::rbindAll(gaugeInfo))
  gaugeInfo <- gaugeInfo[order(gaugeInfo$ID), ]

  structure(rainData, gaugeInfo = gaugeInfo)
}

# .removeDuplicates ------------------------------------------------------------

#' Remove 'Full' and Check for 'Key' Duplicates
#' 
#' @param x data frame with times in first column
#' @param columns.key = names(x) 
#' @param do.stop  = TRUE 
#' @param dbg = TRUE
#' @keywords internal
#' @noRd
#' 
.removeDuplicates <- function(
  x, columns.key = names(x), do.stop = TRUE, dbg = TRUE
)
{
  #x=rainData;columns.key=names(x);do.stop=TRUE

  isDuplicate <- duplicated(x)

  # Remove the fully identical rows
  if (any(isDuplicate)) {

    kwb.utils::catIf(dbg, "** Removing", sum(isDuplicate), "fully identical rows... ")
    x <- x[! isDuplicate, ]
    kwb.utils::catIf(dbg, "ok.\n")
  }

  # Check for remaining duplicates. Rows with the same keys should be equal in
  # all fields. Show the cases where this is not true.
  keydata <- kwb.utils::selectColumns(x, columns.key)

  n.duplicates <- sum(duplicated(keydata))

  if (n.duplicates) {

    unique.keydata <- unique(keydata)

    kwb.utils::catIf(dbg, "** Checking for duplicate keys...\n")

    # Print rows with the same keys but different values
    for (i in seq_len(nrow(unique.keydata))) {

      y <- merge(unique.keydata[i, ], x)

      if (nrow(y) > 1) {

        key <- kwb.utils::collapsed(y[1, columns.key], ";")
        
        kwb.utils::printIf(TRUE, y, paste("\nDifferent values for", key))
      }
    }

    kwb.utils::catIf(dbg, "** Duplicate key check ok.\n")

    text <- sprintf(
      "There are %d key duplicates (see above). Treat them!", n.duplicates
    )

    if (do.stop) {
      
      stop(text, call. = FALSE)
      
    } else {
      
      message(text)
    }
  }

  x
}

# .toDifferences ---------------------------------------------------------------

.toDifferences <- function(x, method = 2, dbg = TRUE)
{
  # Save the attributes
  attributes.in <- attributes(x)

  # All but the first columns must be numeric
  if (! all(sapply(x, is.numeric) == c(FALSE, rep(TRUE, ncol(x) - 1)))) {
    
    utils::str(x)
    
    stop(
      "All but the first columns must be numeric but are not ",
      "(see output of 'str' above)", call. = FALSE
    )
  }

  kwb.utils::catIf(dbg, "Calculating rain height differences per time interval... ")

  if (method == 1) {

    # Calculate the differences for all but the numeric columns
    x[-1] <- lapply(x[-1], function(x) c(NA, diff(x)))

    result <- cbind(
      From = x[-nrow(x), 1],
      To = x[-1, 1],
      x[-1, -1],
      stringsAsFactors = FALSE
    )

  } else {

    x[-1, -1] <- lapply(x[-1], diff)

    result <- cbind(
      From = x[-c(1, nrow(x)), 1],
      To = x[-(1:2), 1],
      x[-c(1, nrow(x)), -1],
      stringsAsFactors = FALSE
    )
  }

  kwb.utils::catIf(dbg, "ok.\n")

  kwb.utils::resetRowNames(kwb.utils::hsRestoreAttributes(
    result, attributes.in
  ))
}

# .toSignals -------------------------------------------------------------------

.toSignals <- function(rainDiff, dbg = TRUE)
{
  kwb.utils::catIf(dbg, "Filtering for 'signals' (non-zero values)... ")

  # Save the attributes of the incoming data frame
  attributes.in <- attributes(rainDiff)

  # Keep only rows where the sum of all gauge values (after excluding NAs) is
  # greater than zero
  signals <- rainDiff[rowSums(rainDiff[, -(1:2)], na.rm = TRUE) > 0, ]

  # Get the date strings from the "From" column
  dates <- substr(signals[, 1], 1, 10)

  # Reduce the "From" and "To" column to the time string HH:MM
  signals[1:2] <- lapply(signals[1:2], function(x) {
    substr(x, 12, nchar(x))
  })

  # Add a new "Date" column to the left
  out <- cbind(Date = dates, signals, stringsAsFactors = FALSE)

  kwb.utils::catIf(dbg, "ok.\n")

  # Restore the attributes that were saved above
  kwb.utils::resetRowNames(kwb.utils::hsRestoreAttributes(out, attributes.in))
}

# .toLongFormat ----------------------------------------------------------------

.toLongFormat <- function(x, type = "")
{
  types <- kwb.utils::toNamedList(c("correction", "signal"))

  args.general <- list(colNamePar = "Gauge", colNameVal = "Value")

  if (type == types$correction) {

    args <- kwb.utils::arglist(args.general, keyFields = "Date")

  } else if (type == types$signal) {

    args <- kwb.utils::arglist(args.general, keyFields = names(x)[1:3])

  } else {

    stop("type must be one of ", kwb.utils::stringList(as.character(types)))
  }

  x.long <- kwb.utils::callWith(kwb.utils::hsMatrixToListForm, args, df = x)

  kwb.utils::moveColumnsToFront(x.long, args$keysFields)
}

# read_rain --------------------------------------------------------------------

#' Read Rain Data from BWB Excel Files
#' 
#' @param directory directory containing excel files from which rain data are
#'   read. Default: \code{kwb.utils::resolve("RAIN_DIR", dictionary)}
#' @param pattern file name pattern matching names of files to be read. Default:
#'   \code{kwb.utils::resolve("RAIN_XLS_PATTERN", dictionary)}
#' @param tableName name of sheet in the excel files containing the rain data.
#'   Default: \code{kwb.utils::resolve("RAIN_XLS_TABLE", dictionary)}
#' @param dictionary list of key/value pairs that can be used as a dictionary to
#'   look up the following keywords (if \emph{directory}, \emph{pattern}, 
#'   \emph{tableName} are not given): RAIN_DIR, RAIN_XLS_PATTERN, RAIN_XLS_TABLE
#' @param rain.xls.files full paths to xls or xlsx files to be read. If given,
#'   these paths are used. Otherwise all files in \emph{directory} matching the
#'   filename \emph{pattern} are used.
#' @param aggregation.interval aggregation interval in seconds. Default: 
#'   settings$rain.aggregation.interval
#' @param settings list that may contain arguments to this function in elements
#'   \code{rain.aggregation.interval} or \code{dictionary}
#' @param use2007Driver passed to
#'   \code{\link{getAggregatedBwbRainDataFromExcelFiles}}
#' 
#' @return data frame with column \code{DateTime} containing timestamps in UTC 
#'   timezone but representing Berlin normal (= winter) time.
#' 
#' @export
#' 
read_rain <- function(
  directory = kwb.utils::resolve("RAIN_DIR", dictionary),
  pattern = kwb.utils::resolve("RAIN_XLS_PATTERN", dictionary),
  tableName = kwb.utils::resolve("RAIN_XLS_TABLE", dictionary),
  dictionary = kwb.utils::selectElements(settings, "dictionary"),
  rain.xls.files = dir(directory, pattern, full.names = TRUE),
  aggregation.interval = kwb.utils::selectElements(
    settings, "rain.aggregation.interval"
  ),
  settings = NULL, use2007Driver = TRUE
)
{
  if (! length(rain.xls.files)) {
    
    stop(
      "There are no files matching '", pattern, "' in ", directory,
      call. = FALSE
    )
  }

  rain.data <- getAggregatedBwbRainDataFromExcelFiles(
    rain.xls.files = rain.xls.files,
    tableName = tableName,
    aggregation.interval = aggregation.interval,
    use2007Driver = use2007Driver
  )

  rain.data
}

# getAggregatedBwbRainDataFromExcelFiles ---------------------------------------

#' Get Aggregated BWB Rain Data From Excel Files
#' 
#' read BWB rain data from multiple XLS files with possible aggregation. 
#' Timestamps given in Local Time are converted to UTC+1 (= always winter time)
#' 
#' @param rain.xls.files vector of full paths to xls files
#' @param tableName name of sheet in Excel file(s) containing the rain data
#' @param aggregation.interval aggregation interval in seconds. Default: NA (=
#'   no aggregation)
#' @param columnNames optional. Vector of names of columns (after renaming with 
#'   \code{\link{gaugeNamesShort}}) to return
#' @param use2007Driver if TRUE, 2007 driver is used even if extension of
#'   \emph{xls} is "xls"
#' @param dbg if \code{TRUE}, debug messages are shown
#' 
#' @return data frame with time column \emph{DateTime}, indicating the begin of
#'   the 5-minutes interval to which the rain height corresponds.
#' 
#' @export
#' 
getAggregatedBwbRainDataFromExcelFiles <- function(
  rain.xls.files, tableName, aggregation.interval = NA, columnNames = NULL,
  use2007Driver = FALSE, dbg = FALSE
)
{
  rain.data <- getBwbRainDataFromExcelFiles(
    xlsPaths = rain.xls.files,
    tableName = tableName,
    columnNames = columnNames,
    use2007Driver = use2007Driver,
    dbg = dbg
  )

  if (kwb.utils::isNullOrEmpty(rain.data)) {
    
    stop("getBwbRainDataFromExcelFiles returned without data!")
  }

  # convert summer time to winter time
  rain.data$tBeg_BWB <- kwb.base::hsST2WT(rain.data$tBeg_BWB)

  rain.data <- kwb.utils::renameColumns(rain.data, list(tBeg_BWB = "DateTime"))
  
  rain.data <- kwb.utils::removeColumns(rain.data, "tEnd_BWB")

  timeStep <- kwb.datetime::getTimestepInSeconds(
    rain.data$DateTime, default = 300
  )
  
  cat("Time step in rain data:", timeStep, "s\n")

  if (! is.na(aggregation.interval) && aggregation.interval > timeStep) {

    cat("aggregating rain data in intervals of", aggregation.interval,
        "seconds... ")

    rain.data <- kwb.base::hsGroupByInterval(
      rain.data, interval = aggregation.interval, offset2 = 0, FUN = sum, 
      limits = FALSE
    )

    cat("ok.\n")
  }

  rain.data
}

# getBwbRainDataFromExcelFiles -------------------------------------------------

#' Get BWB Rain Data From Excel Files
#' 
#' @param xlsPaths (vector of) full path(s) to Excel file(s)
#' @param tableName sheet name, default: "[Niederschlag$]"
#' @param columnNames optional. Vector of names of columns (after renaming with 
#'   \code{\link{gaugeNamesShort}}) to return
#' @param use2007Driver if TRUE, 2007 driver is used even if extension of
#'   \emph{xls} is "xls"
#' @param dbg if \code{TRUE} (default), debug messages are shown
#' 
#' @export
#' 
getBwbRainDataFromExcelFiles <- function(
  xlsPaths, tableName = "Niederschlag$", columnNames = NULL, 
  use2007Driver = FALSE, dbg = FALSE
)
{
  rainData.all <- NULL

  for (xls in xlsPaths) {

    rainData <- readBwbRainData(
      file = xls,
      tableName = tableName,
      use2007Driver = use2007Driver,
      columnNames = columnNames,
      dbg = dbg
    )

    if (kwb.utils::isNullOrEmpty(rainData)) {
      
      message("readBwbRainData did not return data from ", xls, "!")
      
    } else {
      
      rainData.all <- kwb.utils::safeRowBind(rainData.all, rainData)
    }
  }

  rainData.all
}

# readYearlyRainHeightsFromOneCsvFile ------------------------------------------

#' Read Yearly Rain Heights From One Csv File
#' 
#' reads a CSV file containing daily rain heights as provided by BWB (Mario 
#' Grunwald). Example filename: "Niederschlaege_1994__BERICHT.csv". The files 
#' are expected to contain a header of three rows (first row: DATUM/station 
#' names, second row: variable name ["Regenhoehe" or "Regendauer"], third row: 
#' unit ["in mm" or "in mm"])
#' 
#' @param csv full path to csv file
#' @param sep column separator. default: comma ","
#' @param dateformat date format, default: "\%d/\%m/\%Y"
#'   
#' @return data frame with columns...
#' 
#' @export
#' 
readYearlyRainHeightsFromOneCsvFile <- function(
  csv, sep = ",", dateformat = "%d/%m/%Y"
)
{
  stopifnot(length(csv) == 1)

  header <- utils::read.table(
    csv, nrows = 3, sep = sep, stringsAsFactors = FALSE
  )

  cols <- which(header[2, ] == "Regenh\u00F6he")
  
  stations <- niceStationNames(header[1, cols])

  # get nice unit names
  units <- sub("in ", "", header[3, cols])

  stopifnot(all(units == "mm"))

  colClasses <- rep("character", ncol(header))
  
  colClasses[cols] <- "numeric"

  values <- utils::read.table(
    csv, sep = sep, skip = 3, colClasses = colClasses, stringsAsFactors = FALSE
  )

  dates <- data.frame(myDate = as.Date(values[, 1], format = dateformat))

  heights <- stats::setNames(values[, cols], stations)

  structure(cbind(dates, heights), unit = "mm")
}
