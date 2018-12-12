# read_BWB_rain_correction_wide_gauge_group ------------------------------------

#' Read Correction Data
#' 
#' Read Correction Data in "wide" format, grouped by groups of pumping stations
#' 
#' @param file path to rain correction data file
#' @param encoding file encoding, passed to \code{readLines}
#' @param \dots arguments passed to \code{kwb.read:::.extract_wide_gauge_group}
#' 
read_BWB_rain_correction_wide_gauge_group <- function(
  file, encoding = default_encoding(), ...
)
{
  if (length(file) > 1) {
    
    stop(
      "read_BWB_rain_correction_wide_gauge_group() can only handle ", 
      "one file at once!", call. = FALSE
    )
  }
  
  message("Reading ", kwb.utils::hsQuoteChr(basename(file)), "...")
  
  x <- readLines(kwb.utils::safePath(file), encoding = encoding)
  
  # Replace German Umlaute
  x <- kwb.utils::multiSubstitute(x, list("\xfc" = "ue", "\xf6" = "oe"))

  x <- splitIntoBlocks(x, pattern = "Pumpwerksgruppe", 2)
  
  dataList <- lapply(x, .extract_wide_gauge_group, ...)
  #dataList <- lapply(x, kwb.read:::.extract_wide_gauge_group)

  result <- kwb.utils::mergeAll(dataList, by = "Date", all = TRUE, dbg = FALSE)
  
  structure(result, names = gsub("\\.\\d+", "", names(result)))
}

# default_encoding -------------------------------------------------------------
default_encoding <- function()
{
  ifelse(kwb.utils::.OStype() == "windows", "WINDOWS-1252", "UTF-8")
}

# .extract_wide_gauge_group ----------------------------------------------------

.extract_wide_gauge_group <- function(
  xx, sep = ";", date.format = "%d.%m.%Y", country = "en", encoding = "UTF-8"
)
{
  #sep=";";date.format="%d.%m.%Y";country="en";encoding="UTF-8"
  y <- utils::read.table(
    text = xx, sep = sep, encoding = encoding, stringsAsFactors = FALSE
  )
  
  y <- kwb.utils::removeEmptyColumns(y, dbg = FALSE)
  
  .stopOnTooFewColumns(y, 2)
  
  isNoDate <- grepl("^$|^\\D", y[, 1])
  
  # Replace two or more spaces with "@"
  metaInfo <- gsub("\\s{2, }", "@", as.matrix(unique(y[isNoDate, ])))
  
  # Span "multi cells" over multiple columns
  isMultiCell <- grepl("@", metaInfo[, 2])
  
  if (any(isMultiCell)) {
    
    FUN <- function(x) strsplit(x, "@")[[1]]
    
    metaInfo[isMultiCell, -1] <- t(sapply(metaInfo[isMultiCell, 2], FUN))
  }
  
  metaInfo <- unique(metaInfo)
  
  gaugeInfo <- metaInfo[metaInfo[, 1] == "", -1, drop = FALSE]
  
  isConst <- gaugeInfo == "mm" | gaugeInfo == "TREGMM"
  isCaption <- rowSums(isConst) != ncol(gaugeInfo)
  
  stopifnot(sum(isCaption) == 1)  
  
  gauges <- as.character(gaugeInfo[isCaption, ])
  
  data <- y[! isNoDate, ]
  names(data) <- c("Date", gauges)
  
  # Convert character to numeric
  for (i in seq_len(ncol(data))[-1]) {
    
    data[, i] <- kwb.utils::hsChrToNum(data[, i], country = country) 
  }
  
  # Format date
  data$Date <- kwb.datetime::reformatTimestamp(
    data$Date, date.format, "%Y-%m-%d"
  )
  
  # Order by date
  data[order(data$Date), ]
}

# read_BWB_rain_correction_wide_gauge_from_pdf ---------------------------------

#' Read Corrections in "Wide" Format
#' 
#' Read correction data from CSV file(s) in "wide" format with gauges in 
#' columns. Version 2: As generated from PDF-file with "pdf to Excel"
#' 
#' @param file path(s) to CSV file(s)
#' @param sep column separator
#' @param \dots passed to \code{kwb.read:::.extract_wide_gauge}
#'   
read_BWB_rain_correction_wide_gauge_from_pdf <- function(
  file, sep = ";" , ...
)
{
  #splitIntoBlocks <- kwb.read::splitIntoBlocks
  #.extract_wide_gauge <- kwb.read:::.extract_wide_gauge
  #sep=";"
  
  if (length(file) > 1) {
    
    stop(
      "read_BWB_rain_correction_wide_gauge_from_pdf() can only handle one ",
      "file at once!", call. = FALSE
    )
  }
  
  message("Reading ", kwb.utils::hsQuoteChr(basename(file)), "...")
  
  blocks <- splitIntoBlocks(x = readLines(file), pattern = "Berliner", 3)
  
  dataList <- lapply(seq_along(blocks), function(i) {
    
    #cat("Reading block", i, "/", length(blocks), "...\n")
    
    text <- blocks[[i]]
    
    # Remove empty rows (only separator)
    text <- text[! grepl(pattern = paste0("^", sep, "+", "$"), text)]
    text <- text[! grepl(pattern = "Handwerte", text)]
    
    data <- utils::read.table(
      text = text, sep = sep, stringsAsFactors = FALSE, encoding = "UTF-8", 
      colClasses = "character"
    )
    
    if (length(row.header <- grep("Datum", data[, 1])) == 1) {
      
      # Keep only columns "Datum" and "mm", remove the header row
      data <- data[-row.header, data[row.header, ] %in% c("Datum", "mm")]
      
      # Extract the data just as it is done in read_BWB_rain_correction_wide_gauge
      #.extract_wide_gauge(data, "%d.%m.%Y", "en", file, "TRUE")
      .extract_wide_gauge(data, files = file, ...)
      
    } else {
      
      #cat("  skipped.\n")
    }
  })
  
  dataList <- kwb.utils::excludeNULL(dataList)
  
  result <- lapply(dataList, kwb.utils::hsMatrixToListForm, keyFields = "Date")

  result <- kwb.utils::rbindAll(result)
  
  result <- stats::reshape(
    result, direction = "wide", timevar = "parName", idvar = "Date"
  )
  
  structure(
    result[order(kwb.utils::selectColumns(result, "Date")), ],
    names = gsub("parVal\\.", "", names(result)),
    reshapeWide = NULL
  )
}

# splitIntoBlocks --------------------------------------------------------------

#' Split Character Vector Into Blocks of Lines
#' 
#' @param x vector of lines of character
#' @param pattern pattern maching the start lines of the blocks
#' @param offset.start offset added to the indices matching the block starts to
#'   identify the first lines of the blocks to be cut
#' @param offset.stop offset defining the end of the blocks. A value of zero 
#'   (default) means that a block ends one line before the start of the next 
#'   block (a line matching \code{pattern})
#' 
splitIntoBlocks <- function(x, pattern, offset.start = 0, offset.stop = 0)
{
  i.start <- grep(pattern, x)
  
  if (! length(i.start)) {
    
    stop(
      "Pattern ", kwb.utils::hsQuoteChr(pattern), " not found!", call. = FALSE
    )
  }
  
  blockInfo <- data.frame(
    i.start = i.start + offset.start, 
    i.stop = c(i.start[-1] - 1 - offset.stop, length(x))
  )
  
  lapply(seq_len(nrow(blockInfo)), function(i) {
    
    x[blockInfo[i, "i.start"]:blockInfo[i, "i.stop"]]
  })
}

# read_BWB_rain_correction_wide_gauge ------------------------------------------

#' Read Corrections in "wide" format
#' 
#' Read correction data from CSV file(s) in "wide" format with gauges in columns
#' 
#' @param files paths to rain correction files
#' @param sep column separator
#' @param country one of "de" (German) or "en" (English) according to the format
#'   numeric strings are given in
#' @param date.format format used to convert the date string into a date object
#' @param dbg logical. If \code{TRUE}, debug messages are shown.
#'
#' @export
#' 
read_BWB_rain_correction_wide_gauge <- function(
  files, sep = ";", country = "de", 
  date.format = c("%d.%m.%Y", "%A %d. %B %Y")[1], dbg = TRUE
)
{
  #sep=";";country="en";dbg=TRUE
  #.stopOnTooFewFiles <- kwb.read:::.stopOnTooFewFiles
  #.stopOnTooFewColumns <- kwb.read:::.stopOnTooFewColumns
  #.convertToDateStrings <- kwb.read:::.convertToDateStrings
  
  # files must be character
  stopifnot(is.character(files))

  # If more than one file is given, call this function for each file, merge
  # the results and return
  if (length(files) > 1) {

    dataList <- lapply(files, read_BWB_rain_correction_wide_gauge, sep, country,
                       date.format, dbg)

    data <- kwb.utils::safeRowBindAll(dataList)
    gauges <- .mergeGaugeLists(dataList)
    sums <- .mergeSumsLists(dataList)

    return (structure(data, sums = sums, files = files))
  }

  .stopOnTooFewFiles(files)

  message("Reading ", kwb.utils::hsQuoteChr(basename(files[1])), "...")

  data <- utils::read.table(
    files, sep = sep, stringsAsFactors = FALSE, encoding = "UTF-8", 
    colClasses = "character"
  )
  
  .extract_wide_gauge(data, date.format, country, files, dbg)
}

# .extract_wide_gauge ----------------------------------------------------------

.extract_wide_gauge <- function(data, date.format, country, files, dbg)
{
  # Remove empty columns and rows
  allEmpty <- function(x) all(kwb.utils::isNaOrEmpty(x))
  
  data <- kwb.utils::removeEmptyColumns(
    data, FUN = allEmpty, drop = FALSE, dbg = FALSE
  )
  
  .stopOnTooFewColumns(data, 3, n.rows = 5)
  
  data <- data[! apply(data, 1, FUN = allEmpty), ]
  
  # The first two rows are expected to be header rows. Exclude the "Sum" column
  isSum <- (data[1, ] == "Sum")
  
  if (any(isSum)) {
    
    kwb.utils::catIf(dbg, "Excluding sum column... ")
    data <- data[, ! isSum]
    kwb.utils::catIf(dbg, "ok.\n")
  }
  
  gauges <- data.frame(
    ObjNr = as.character(data[1, -1]),
    Pumpwerk = as.character(data[2, -1]),
    stringsAsFactors = FALSE
  )
  
  isDuplicated <- duplicated(gauges$ObjNr)
  
  if (any(isDuplicated)) {
    
    duplicates <- gauges$ObjNr[isDuplicated]
    
    gauges <- gauges[! isDuplicated, ]
    data <- data[, - (which(isDuplicated) + 1)]
    
    warning(
      "Columns with duplicated ObjNr were removed in ",
      kwb.utils::hsQuoteChr(basename(files)), ": ",
      kwb.utils::stringList(duplicates), call. = FALSE
    )
  }
  
  # Remove the first two rows
  data <- data[-(1:2), ]
  
  # Name the columns
  names(data) <- c("Date", gauges$ObjNr)
  
  # Convert all but the first column to numeric
  data[-1] <- lapply(data[-1], kwb.utils::hsChrToNum, country)
  
  # Last row is expected to contain the total sums
  i.last <- nrow(data)
  
  if (! data[i.last, 1] %in% c("", "Summe:")) {
    
    stop(
      "The first field of the last row is not empty as expected but: ",
      kwb.utils::hsQuoteChr(data[i.last, 1]), " in ",
      kwb.utils::hsQuoteChr(basename(files)),
      call. = FALSE
    )
  }
  
  sums <- kwb.utils::resetRowNames(data[i.last, -1])
  
  # Exclude the sums
  data <- kwb.utils::resetRowNames(data[-i.last, ])
  
  # If the datestrings look like numerics, treat the number as the day number
  # since 1899-12-30 (as it is done in Excel)
  data[, 1] <- .convertToDateStrings(data[, 1], format = date.format)
  
  # Check if the sums are correct
  columnSums <- colSums(data[, -1], na.rm = TRUE)
  
  isEqual <- kwb.utils::almostEqual(
    x = as.numeric(columnSums),
    y = kwb.utils::defaultIfNA(as.numeric(sums), 0.0)
  )
  
  if (! all(isEqual)) {
    
    info <- data.frame(
      Pumpwerk = names(sums)[! isEqual],
      sum_CSV = as.numeric(sums)[! isEqual],
      sum_R = as.numeric(columnSums)[! isEqual]
    )
    
    message("Different sums in ", kwb.utils::hsQuoteChr(basename(files)))
    
    print(info)
    
    warning(
      "The sums given in the file do not match the sums calculated in R ",
      "(in ", kwb.utils::hsQuoteChr(basename(files)), "), see above!", 
      call. = FALSE
    )
  }
  
  structure(data, sums = sums, gauges = gauges, files = files)
}

# .stopOnDuplicateGauges -------------------------------------------------------

.stopOnDuplicateGauges <- function(gauges, groups = NULL, file)
{
  duplicates <- if (is.null(groups)) {

    gauges[duplicated(gauges)]

  } else {

    maxFrequency <- apply(table(groups, gauges), 2, max)
    
    names(maxFrequency)[maxFrequency > 1]
  }

  if (length(duplicates)) {

    stop(
      "There are duplicated gauges in ", kwb.utils::hsQuoteChr(file), ":",
      kwb.utils::stringList(duplicates), call. = FALSE
    )
  }
}

# read_BWB_rain_correction_wide_day --------------------------------------------

#' Read Correction "Wide" Format
#' 
#' Read correction data from CSV file(s) in "wide" format with days in columns
#' 
#' @param files paths to rain correction files
#' @param sep column separator
#' @param country one of "de" (German) or "en" (English) according to the format
#'   numeric strings are given in
#' @param transpose logical. If \code{TRUE} (default) data are transposed so 
#'   that the different rain gauges appear in different columns
#' @param dbg logical. If \code{TRUE}, debug messages are shown.
#' @param date.format format used to convert the date string into a date object
#' @param encoding Encoding used when reading the CSV file. Best would be to use
#'   "UTF-8" but MS Excel uses system default
#'   
#' @export
#' 
read_BWB_rain_correction_wide_day <- function(
  files, sep = ";", country = "de", transpose = TRUE, dbg = TRUE, 
  date.format = c("%d %B %Y")[1], encoding = ""
)
{
  #sep=";";country="de";transpose=TRUE;dbg=TRUE

  # files must be character
  stopifnot(is.character(files))

  # If more than one file is given, call this function for each file, merge
  # the results and return
  if (length(files) > 1) {

    dataList <- lapply(
      files, read_BWB_rain_correction_wide_day, sep, country, transpose, dbg
    )

    return (structure(
      data = kwb.utils::safeRowBindAll(dataList), 
      gauges = .mergeGaugeLists(dataList), 
      means = .mergeMeansLists(dataList), 
      files = files
    ))
  }

  #kwb.read:::.stopOnTooFewFiles(files)
  .stopOnTooFewFiles(files)

  message("Reading ", kwb.utils::hsQuoteChr(basename(files[1])), "...")

  data <- utils::read.table(
    files, sep = sep, stringsAsFactors = FALSE, encoding = encoding,
    colClasses = "character"
  )

  allEmpty <- function(x) all(kwb.utils::isNaOrEmpty(x))
  
  data <- kwb.utils::removeEmptyColumns(data, FUN = allEmpty, dbg = FALSE)

  .stopOnTooFewColumns(data, 5)

  keyword  <- "Niederschlag Monatsbericht"
  
  datecolumn <- grep(keyword, data[2, ])

  if (! length(datecolumn)) {
    
    stop(
      "Keyword '", keyword, "' not found in second row. ", .text.wrong.format(),
      call. = FALSE
    )
  }

  # Find the rows containing the year and month information
  blockstarts <- which(data[, 1] == (keyword <- "Wasserbetriebe"))

  if (! length(blockstarts)) {
    
    stop(
      "No row contains the keyword '", keyword, "' in the first column! ",
      .text.wrong.format(), call. = FALSE
    )
  }

  # Generate information about the different blocks of rows each of which
  # contains the data for one month (start index, stop index, number of
  # columns)
  blockInfo <- data.frame(
    i.start = (i <- blockstarts + 3),
    i.stop = c(i[-1] - 1, nrow(data)),
    n.col = sapply(i, function(ii) max(which(data[ii, ] != "")))
  )

  # Within each block of rows, move the last column within the block (Sum) to
  # the last column of data
  n.col <- ncol(data)

  for (i in seq_len(nrow(blockInfo))) {

    if (blockInfo$n.col[i] < n.col) {

      i.range <- blockInfo$i.start[i]:blockInfo$i.stop[i]
      j.last <- blockInfo$n.col[i]
      data[i.range, n.col] <- data[i.range, j.last]
      data[i.range, j.last] <- ""
    }
  }

  # Name the columns
  dayColumns <- sprintf("d%02d", seq_len(ncol(data) - 3))
  names(data) <- c("ObjNr", "Pumpwerk", dayColumns, "Summe")

  # Extract the date strings
  dates <- data[blockstarts, datecolumn]

  if (all(kwb.utils::isNaOrEmpty(dates))) {

    stop(
      "No dates found in the ", datecolumn, "-th non-empty column. ",
      .text.wrong.format(), call. = FALSE
    )
  }

  # If the datestrings look like numerics, treat the number as the day number
  # since 1899-12-30 (as it is done in Excel)
  dates <- .convertToDateStrings(dates, format = date.format, prefix = "1 ")

  # Append a date column and fill the gaps "down"
  monthColumn <- "Monat"
  data[, monthColumn] <- NA
  data[blockstarts, monthColumn] <- substr(dates, 1, 7)
  data[, monthColumn] <- kwb.utils::naToLastNonNa(data[, monthColumn])

  # Extract the mean values
  means <- data[data[, 2] == "MW:", c(monthColumn, dayColumns)]
  means <- kwb.utils::resetRowNames(means)
  means[-1] <- lapply(means[, -1], kwb.utils::hsChrToNum, country = country)

  # Filter for the data rows
  data <- data[grepl("^\\d\\d\\.\\d\\d$", data[, 1]), ]
  data <- kwb.utils::resetRowNames(data)

  # Stop if there are gauge duplicates wihin the same month
  .stopOnDuplicateGauges(data$ObjNr, groups = data[, monthColumn], files)

  columns <- c(monthColumn, names(data)[1:2], "Summe")
  data <- kwb.utils::moveColumnsToFront(data, columns)

  # Convert the rain height columns to numeric
  columns <- c(dayColumns, "Summe")
  data[columns] <- lapply(data[columns], kwb.utils::hsChrToNum, country)

  # Provide the mapping between object numbers and gauge names
  gauges <- unique(data[, 2:3])
  gauges <- kwb.utils::resetRowNames(gauges[order(gauges[, 1]), ])

  # Check if the sums are correct
  sums <- rowSums(data[, 5:ncol(data)], na.rm = TRUE)
  sums.orig <- kwb.utils::selectColumns(data, "Summe")

  if (! all(kwb.utils::almostEqual(sums, sums.orig), na.rm = TRUE)) {

    warning(
      "The sums in the CSV file do not match the recalculated sums in ",
      kwb.utils::hsQuoteChr(files), call. = FALSE
    )
  }

  if (transpose) {

    kwb.utils::catIf(dbg, "Transposing... ")
    
    data <- .toGaugeInColumns(data)
    
    kwb.utils::catIf(dbg, "ok.\n")
  }

  # Order value columns by ObjNr
  columns <- names(data)
  data <- data[, c(columns[1], sort(columns[-1]))]

  structure(data, gauges = gauges, means = means, files = files)
}

# .toGaugeInColumns ------------------------------------------------------------

.toGaugeInColumns <- function(data)
{
  INDICES <- factor(kwb.utils::selectColumns(data, "Monat"))

  blocks <- by(data, INDICES, function(x) {
    
    x <- kwb.utils::removeEmptyColumns(x, dbg = FALSE)
    values <- t(x[, -c(1:4)])
    dayNumber <- substr(row.names(values), 2, 3)
    dates <- paste(unique(x$Monat), dayNumber, sep = "-")
    out <- data.frame(dates, values, stringsAsFactors = FALSE)
    structure(kwb.utils::resetRowNames(out), names = c("Date", x$ObjNr))
  })

  kwb.utils::safeRowBindAll(blocks)
}
