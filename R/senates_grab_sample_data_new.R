# read_SenatesGrabSampleCsvFiles_wide ------------------------------------------

#' Read Senates' Grab Sample Files
#' 
#' Read Senates' grab sample files ('wide' format). For a format description see
#' \code{\link{hsGetGsData2}}
#' 
#' @param csvs vector of paths to CSV files
#' @param sep column separator
#' @param dateFormat date format string, passed to
#'   \code{\link{read_SenatesGrabSampleCSV_wide}} and
#'   \code{kwb.read:::.doConversion}
#' @param country one of "de" (German) or "en" (English) according to the format
#'   numeric strings are given in
#' @param dbg logical. If \code{TRUE}, debug messages are shown.
#' @param oldversion passed to \code{kwb.read:::.doConversion}
#' 
#' @export
#' 
read_SenatesGrabSampleCsvFiles_wide <- function(
  csvs, sep, dateFormat, country = "de", dbg = TRUE, oldversion = FALSE
)
{  
  result <- lapply(
    csvs, read_SenatesGrabSampleCSV_wide, sep = sep, dateFormat = dateFormat, 
    country = country, doConversion = FALSE, dbg = dbg
  )
  
  result <- kwb.utils::rbindAll(result)
  
  result <- .doConversion(
    x = result, dateFormat = dateFormat, country = country, dbg = dbg,
    oldversion = oldversion
  )
  
  kwb.utils::resetRowNames(result)
}

# read_SenatesGrabSampleCSV_wide -----------------------------------------------

#' Read Senates' Grab Sample File
#' 
#' Read Senates' grab sample file ('wide' format). For a format description see 
#' \code{\link{hsGetGsData2}}
#' 
#' @param csv path to CSV file
#' @param sep column separator
#' @param dateFormat date format string, passed to
#'   \code{kwb.read:::.doConversion} if \code{doConversion} is \code{TRUE}
#' @param country one of "de" (German) or "en" (English) according to the format
#'   numeric strings are given in
#' @param doConversion logical. If \code{TRUE}, the function
#'   \code{kwb.read:::.doConversion} is called on the result
#' @param dbg logical. If \code{TRUE}, debug messages are shown.
#' @param oldversion passed to \code{kwb.read:::.doConversion} if
#'   \code{doConversion} is \code{TRUE}
#'
read_SenatesGrabSampleCSV_wide <- function(
  csv, sep, dateFormat, country = "de", doConversion = TRUE, dbg = TRUE, 
  oldversion = FALSE
)
{
  dataFrame <- .hsReadAndCheck(csv, sep)
  
  # empty rows are already removed -> set offsets accordingly
  dataBlocks <- kwb.utils::extractRowRanges(
    dataFrame = dataFrame, columnName = "V1", pattern = PATTERN_STATION(),
    startOffset = 1, stopOffset = 1, nameByMatch = TRUE, 
    nameColumnsByMatch = FALSE, renumber = FALSE
  )
  
  kwb.utils::catIf(dbg, "Extracting data blocks... ")
  
  x <- lapply(dataBlocks, function(dataBlock) {
    
    headerRow <- dataBlock[1, ]
    
    columns <- seq_len(.get_ncol(headerRow = headerRow))    
    
    dataBlock <- dataBlock[-1, columns]
    
    names(dataBlock) <- c("smpDate", as.character(headerRow[columns[-1]]))
    
    kwb.utils::hsMatrixToListForm(
      dataBlock, keyFields = "smpDate", colNamePar = "parameter"
    )
  })
  
  x <- kwb.utils::rbindAll(x, nameColumn = "station")
  
  kwb.utils::catIf(dbg, "ok.\n")
  
  if (doConversion) {
    
    x <- .doConversion(
      x, dateFormat = dateFormat, country = country, dbg = dbg, 
      oldversion = oldversion
    )
  }
  
  x
}

# .get_ncol --------------------------------------------------------------------

.get_ncol <- function(headerRow)
{
  max(which(! kwb.utils::isNaOrEmpty(headerRow)))
}

# .doConversion ----------------------------------------------------------------

.doConversion <- function(
  x, dateFormat, country, dbg = TRUE, oldversion = FALSE
)
{
  kwb.utils::checkForMissingColumns(
    x, c("smpDate", "parameter", "parVal", "station")
  )
  
  x$smpDate <- as.Date(x$smpDate, format = dateFormat)
  
  x <- .convertTextToNumeric(x, country, oldversion, dbg)
  
  x <- .appendInfoColumns(
    x, "station", FUN = .hsMoniPointInfo, pattern = PATTERN_STATION(), dbg = dbg
  )
  
  x <- .appendInfoColumns(
    x, "parameter", FUN = .hsParInfo, pattern = PATTERN_PARAMETER(), dbg = dbg
  )

  x[, c("mpID", "parID", "parName", "parUnit", "smpDate", "parVal", "parLim")]
}

# .convertTextToNumeric --------------------------------------------------------

.convertTextToNumeric <- function(x, country, oldversion = FALSE, dbg = TRUE)
{
  kwb.utils::catIf(dbg, "Converting parameter values from text to numeric ... ")
  
  x <- cbind(
    kwb.utils::removeColumns(x, "parVal"),
    do.call(
      what = ifelse(oldversion, "hsLabValToVal_old", "hsLabValToVal"), 
      args = list(
        x = x$parVal, country = country, detLimFactorBelow = 1,
        detLimFactorAbove = 1, stopOnError = FALSE
      )
    )
  )
  
  kwb.utils::catIf(dbg, "ok.\n")

  kwb.utils::renameColumns(x, list(
    numericValue = "parVal", outOfLimit = "parLim"
  ))
}

# .appendInfoColumns -----------------------------------------------------------

.appendInfoColumns <- function(x, column, FUN, ..., dbg = TRUE)
{
  kwb.utils::catIf(dbg, "Appending info columns for", column, "... ")

  # Provide row number for restoring the original row order after merging
  x$ROW_ORDER <- seq_len(nrow(x))
  
  # Provide the values to be looked up
  x$LOOKUP <- as.factor(x[[column]])
  
  # Create a lookup table by calling the given function with the additional
  # arguments
  lookupLevels <- levels(x$LOOKUP)
  
  info <- cbind(LOOKUP = lookupLevels, FUN(lookupLevels, ...))
  
  # Merge the data frame with the lookup table
  x <- merge(x, info, all.x = TRUE)
  
  kwb.utils::catIf(dbg, "ok.\n")

  # Restore the original row order and remove helper columns
  kwb.utils::removeColumns(x[order(x$ROW_ORDER), ], c("ROW_ORDER", "LOOKUP"))
}

# read_SenatesGrabSampleCsvFiles_long ------------------------------------------

#' Read Senates' Grab Sample Files
#' 
#' read Senates' grab sample files ('long' format). For a format description see
#' \code{\link{hsGetGsData1}}
#' 
#' @param csvFiles full paths to CSV files to be read
#' @param \dots arguments passed to \code{\link{read_SenatesGrabSampleCSV_long}}
#'
#' @export
#' 
read_SenatesGrabSampleCsvFiles_long <- function(csvFiles, ...)
{
  result <- lapply(csvFiles, read_SenatesGrabSampleCSV_long, ...)
  
  kwb.utils::rbindAll(result, nameColumn = "file")
}

# read_SenatesGrabSampleCSV_long -----------------------------------------------

#' Read Senates' Grab Sample File
#' 
#' Read Senates' grab sample file ('long' format). For a format description see 
#' \code{\link{hsGetGsData1}}
#' 
#' @param csv path to CSV file
#' @param sep column separator
#' @param dateFormat date format string, passed to
#'   \code{kwb.read:::.removeNonDataRows} and
#'   \code{kwb.read:::.convertDataTypes}
#' @param dec decimal character
#' @param dbg logical. If \code{TRUE}, debug messages are shown.
#' 
read_SenatesGrabSampleCSV_long <- function(
  csv, sep = kwb.utils::guessSeparator(csv), dateFormat, 
  dec = ifelse(sep == ",", ".", ","), dbg = FALSE
)
{
  dataFrame <- .hsReadAndCheck(csv, sep = sep, dbg = dbg)
  
  dataFrame <- kwb.utils::renameColumns(dataFrame, list(
      V1 = "smpDate", V2 = "parTxt", V3 = "parLim"
  ))
  
  config <- list(
    station = list(pattern = PATTERN_STATION(), column = "smpDate"),
    parameter = list(pattern = PATTERN_PARAMETER(), column = "smpDate"),
    series = list(pattern = PATTERN_DATA(), column = "parTxt")
  )
  
  dataFrame <- .appendKeyColumns(dataFrame, config, dbg = dbg)
  
  dataFrame <- .removeKeyRows(dataFrame, config)
  
  dataFrame <- .removeNonDataRows(dataFrame, dateFormat = dateFormat)
  
  dataFrame <- .convertDataTypes(dataFrame, dateFormat = dateFormat, dec = dec)
  
  dataFrame <- .appendInfoColumns(
    dataFrame, "station", FUN = .hsMoniPointInfo, dbg = dbg
  )
  
  dataFrame <- .appendInfoColumns(
    dataFrame, "parameter", FUN = .hsParInfo, dbg = dbg
  )  
  
  columns <- c(
    "series", "mpID", "mpName", "parID", "parName", "parUnit", "smpDate", 
    "parTxt", "parVal", "parLim"
  )
  
  dataFrame[, columns]
}

# .appendKeyColumns ------------------------------------------------------------

.appendKeyColumns <- function(dataFrame, config, dbg = TRUE)  
{
  kwb.utils::catIf(dbg, "Appending key columns... ")
  
  for (key in names(config)) {
    
    pattern <- config[[key]]$pattern
    
    column  <- config[[key]]$column
    
    keyIndices <- grep(pattern, dataFrame[, column])
    
    dataFrame[, key] <- rep(NA, nrow(dataFrame))
    
    dataFrame[keyIndices, key] <- dataFrame[keyIndices, column]
    
    dataFrame[, key] <- kwb.utils::naToLastNonNa(dataFrame[, key])
  }
  
  kwb.utils::catIf(dbg, "ok.\n")
  
  dataFrame
}

# .removeKeyRows ---------------------------------------------------------------

.removeKeyRows <- function(dataFrame, config)
{
  removeAt <- unlist(sapply(config, function(x) {
    
    grep(x$pattern, dataFrame[, x$column])
  }))
  
  if (length(removeAt) > 0) {
    
    dataFrame <- dataFrame[- removeAt, ]
  }
  
  dataFrame
}

# .removeNonDataRows -----------------------------------------------------------

.removeNonDataRows <- function(dataFrame, dateFormat, dbg = TRUE)
{
  removeAt <- which(! kwb.datetime::hasTimeFormat(
    dataFrame$smpDate, dateFormat
  ))
  
  if (length(removeAt) > 0) {
    
    kwb.utils::printIf(
      dbg, dataFrame[removeAt, ], 
      sprintf("Removing %d non-data rows", length(removeAt))
    )
    
    dataFrame <- dataFrame[- removeAt, ]
  }
  
  dataFrame
}

# .convertDataTypes ------------------------------------------------------------

.convertDataTypes <- function(dataFrame, dateFormat, dec, dbg = TRUE)
{
  kwb.utils::catIf(dbg, "Converting data types... ")
  
  dataFrame$smpDate <- kwb.utils::hsStringToDate(dataFrame$smpDate, dateFormat)
  
  dataFrame$parVal <- kwb.utils::hsStringToDouble(dataFrame$parTxt, dec)
  
  kwb.utils::catIf(dbg, "ok.\n")
  
  dataFrame
}
