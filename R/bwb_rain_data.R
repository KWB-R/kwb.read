# readBwbRainData --------------------------------------------------------------

#' Read BWB Rain Data
#' 
#' @param file full path to Excel file
#' @param tableName sheet name, default: "[Niederschlag$]"
#' @param columnNames optional. Vector of names of columns (after renaming with 
#'   \code{\link{gaugeNamesShort}}) to return
#' @param toUTC default: TRUE
#' @param toNormaltime if TRUE, a column \emph{tBeg_WT} is created containing
#'   the begin of the time interval in Normal time (winter time, UTC+01).
#'   default: FALSE
#' @param dbg default: TRUE
#' @param use2007Driver if TRUE, 2007 driver is used even if extension of
#'   \emph{file} is ".xls"
#' @param \dots additional parameters passed to \code{\link{readBwbRainFromCsv}}
#' 
#' @export
#' 
readBwbRainData <- function(
  file, tableName = "Niederschlag$", columnNames = NULL, toUTC = TRUE,
  toNormaltime = FALSE, dbg = TRUE, 
  use2007Driver = kwb.db::isExcel2007File(file), ...
)
{
  timeColumns <- c("tBeg_BWB", "tEnd_BWB")

  ## Get the table
  cat("Getting data from", file, "... ")

  if (kwb.utils::stringEndsWith(file, ".csv")) {

    rainData <- readBwbRainFromCsv(file, ...)
    
    isNA <- names(rainData) == "NA"
    
    names(rainData)[which(isNA)] <- timeColumns[seq_len(sum(isNA))]

  } else {

    rainData <- kwb.db::hsGetTable(
      mdb = file, tbl = tableName, as.is = toNormaltime, dbg = dbg, 
      check = TRUE, use2007Driver = use2007Driver
    )

    renames <- kwb.utils::toLookupList(
      paste0("F", seq_along(timeColumns)), timeColumns
    )
    
    rainData <- kwb.utils::renameColumns(rainData, renames)
  }

  cat("ok.\n")

  ## Rename the columns
  names(rainData) <- gaugeNamesShort(colnames = names(rainData))

  # all but the first two columns should be numeric now...
  #rainData$BlnX <- as.numeric(gsub(",", ".", rainData$BlnX))

  # Remove rows where there is no timestamp
  selected <- ! is.na(kwb.utils::selectColumns(rainData, "tBeg_BWB"))
  
  rainData <- rainData[selected, ]

  ## Remove NA rows (e.g. second header line originally containing units "mm"
  ## and possible NA-lines at the end)
  rainData <- kwb.base::hsDelNaRowsOrCols(rainData)

  ## Find rows in which we have only timestamps but no values at all
  exclude <- grepl("^(tBeg|tEnd)", names(rainData))
  
  na.rows <- which(kwb.utils::isNaInAllColumns(rainData[, ! exclude]))

  if (! kwb.utils::isNullOrEmpty(na.rows)) {

    print(utils::head(rainData[na.rows, ], 3))
    
    cat("...\n")

    print(utils::tail(rainData[na.rows, ], 3))
    
    cat("\n\n*** The above rows contain NA in all columns!\n\n")

    breaks <- kwb.utils::breakInSequence(na.rows)

    lastValidRow <- if (kwb.utils::isNullOrEmpty(breaks)) {
      
      # the sequence of row numbers does not have a break
      na.rows[1] - 1
      
    } else {
      
      # the sequence of row numbers has breaks
      na.rows[kwb.utils::lastElement(breaks)]
    }

    kwb.utils::printIf(TRUE, rainData[lastValidRow, ], "last valid row")

    rainData <- rainData[seq_len(lastValidRow), ]
  }

  # if column names are given, # all column names must exist!
  if (! is.null(columnNames)) {

    missingColumns <- setdiff(columnNames, names(rainData))

    if (! kwb.utils::isNullOrEmpty(missingColumns)) {

      stop(
        "The following column names were not found:",
        kwb.utils::stringList(missingColumns),
        "\nAvailable column names:",
        kwb.utils::stringList(names(rainData)[-(1:2)])
      )
    }

    # Select columns
    rainData <- rainData[, c(timeColumns, columnNames)]
  }

  if (toNormaltime) {
    ## if "toISO" is active, timestamps have been returned as characters!

    ## Convert summer time to winter time (use UTC, knowing that in fact
    ## UTC+01:00 is meant!)
    tUTCplus1 <- kwb.base::hsST2WT(kwb.datetime::hsToPosix(rainData$tBeg_BWB))

    ## Set +01:00 manually to generate the correct ISO-timstamp
    #rainData$tISO <- paste(format(tUTCplus1, "%Y-%m-%dT%H:%M:%S"), "+01:00", sep = "")
    rainData$tBeg_WT <- format(tUTCplus1)
  }

  ## "hardcode" timezone to UTC, thus taking timestamps as they are and
  ## preventing from any time conversion
  if (toUTC) {

    for (column in names(rainData)[exclude]) {
      
      rainData[[column]] <- kwb.datetime::hsToPosix(
        kwb.utils::selectColumns(rainData, column)
      )
    }
  }

  kwb.utils::printIf(dbg, summary(rainData))

  rainData
}

# readBwbRainFromCsv -----------------------------------------------------------

#' Read BWB Rain Data from CSV File
#' 
#' read BWB rain data from a CSV file that has been created by saving the 
#' corresponding Excel file as CSV
#' 
#' @param file path to CSV file
#' @param sep column separator
#' @param dec decimal character
#' @param format date format string, passed to \code{\link{as.POSIXct}}
#' 
readBwbRainFromCsv <- function(
  file, sep = ";", dec = ",", format = "%d.%m.%y %H:%M:%S"
)
{
  captions <- utils::read.csv(
    text = readLines(file, n = 1), sep = sep, header = FALSE,
    stringsAsFactors = FALSE
  )

  captions <- as.character(captions[1, ])

  isTimeColumn <- captions == "NA"

  colClasses <- rep("numeric", length(captions))
  colClasses[isTimeColumn] <- "character"

  rainData <- utils::read.table(
    file = file, header = FALSE, skip = 2, sep = sep,
    dec = dec, colClasses = colClasses,
    na.strings = "[-11059]NoGoodDataForCalculation"
  )

  for (i in which(isTimeColumn)) {

    timestamps <- rainData[, i]

    valid <- kwb.datetime::hasTimeFormat(timestamps, format)

    if (any(! valid)) {
      
      stop(call. = FALSE, sprintf(
        "There are timestamps not matching the format '%s': %s. %s",
        format, paste(collapse = ", ", kwb.utils::hsQuoteChr(utils::head(
          timestamps[! valid]
        ))),
        "You may specify the format by setting the argument 'format'."
      ))
    }

    rainData[, i] <- as.POSIXct(timestamps, format = format)
  }

  structure(rainData, names = captions)
}

# readBwbRainFromCsv2 ----------------------------------------------------------

#' Read BWB rain data from CSV file
#' 
#' @param file full path to CSV file
#' @param sep column separator
#' @param country "en" for English number format, "de" for German number format
#' @param date.format date format string, passed to
#'   \code{\link[kwb.datetime]{reformatTimestamp}}
#' @param dbg if \code{TRUE} (default), debug messages are shown
#' 
#' @export
#' 
readBwbRainFromCsv2 <- function(
  file, sep = ";", country = "de", date.format = "%d.%m.%Y %H:%M", dbg = TRUE
)
{
  #sep=";";country="en";dbg=TRUE
  kwb.utils::catIf(dbg, "** Reading", basename(file), "... ")
  
  rainData <- utils::read.csv(
    file, sep = sep, colClasses = "character", stringsAsFactors = FALSE,
    na.strings = "[-11059] No Good Data For Calculation", header = FALSE
  )
  
  kwb.utils::catIf(dbg, "ok.\n")

  # Remove empty columns
  rainData <- kwb.utils::removeEmptyColumns(rainData, dbg = FALSE)

  # Check header lines for validity
  stopifnot(all(unlist(rainData[3, ])[-1] == "mm"))

  # Extract meta information about the gauges from the first two rows. Exclude
  # the first (timestamp) column
  gaugeInfo <- data.frame(t(rainData[1:2, -1]), stringsAsFactors = FALSE)
  names(gaugeInfo) <- c("ID", "Longname")

  # Set preliminary column names
  names(rainData) <- c("Time", gaugeInfo$ID)

  # Remove header lines
  rainData <- rainData[-(1:3), ]

  # Extend gauge info table by columns "Code" and "Name"
  gaugeInfo <- merge(gaugeInfo, toGaugeInfo(gaugeInfo$Longname))
  gaugeInfo <- gaugeInfo[order(gaugeInfo$ID), ]
  gaugeInfo <- kwb.utils::moveColumnsToFront(gaugeInfo, "ID")

  isDuplicated <- duplicated(gaugeInfo$Code)

  if (any(isDuplicated)) {
    
    stop(
      call. = FALSE, "duplicated gauge IDs: ",
      kwb.utils::stringList(gaugeInfo$gaugeShort[isDuplicated])
    )
  }

  # Rename the columns ID -> Code
  renames <- kwb.utils::toLookupList(gaugeInfo$ID, gaugeInfo$Code)
  rainData <- kwb.utils::renameColumns(rainData, renames)

  # Remove rows without timestamp
  selected <- kwb.utils::matchesCriteria(rainData, "Time != ''", dbg = FALSE)
  rainData <- rainData[selected, ]

  # Provide UTC Offset
  timestamps <- rainData$Time
  timestamps <- kwb.datetime::reformatTimestamp(timestamps, date.format)
  stopifnot(all(! is.na(timestamps)))

  times.local <- kwb.datetime::hsToPosix(timestamps)
  times.normal <- kwb.base::hsST2WT(times.local)
  times.diff <- as.integer(as.double(times.local) - as.double(times.normal))

  utcOffset <- 1 + (times.diff %/% 3600)

  timestamps <- sprintf("%s+%02d", substr(timestamps, 1, 16), utcOffset)

  # There should not be duplicated timestamps within one and the same file!
  isDuplicate <- duplicated(timestamps)

  if (any(isDuplicate)) {

    stop(
      call. = FALSE, "There are duplicate timestamps in ", 
      kwb.utils::hsQuoteChr(file), ":",
      kwb.utils::stringList(timestamps[isDuplicate])
    )
  }

  rainData[, 1] <- timestamps

  # Convert value columns to numeric
  exclude <- 1
  
  rainData[- exclude] <- lapply(
    rainData[- exclude], kwb.utils::hsChrToNum, country = country
  )

  structure(rainData, gaugeInfo = gaugeInfo)
}
