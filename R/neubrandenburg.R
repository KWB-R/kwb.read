# read_Neubrandenburg_Data -----------------------------------------------------

#' Join Data from Multiple Excel Files
#' 
#' @param xls.dir path to directory containing Excel files to be read
#' @param area.Gruendach.m2 area of green roof in square metres. Used to 
#'   convert between litres and litres per square metres (mm) as given in the
#'   result data frame
#' @param area.Kiesdach.m2 area of pebble roof in square metres. Used to 
#'   convert between litres and litres per square metres (mm) as given in the
#'   result data frame
#' @param pattern pattern matching the names of files in \code{xls.dir} to be 
#'   imported
#' 
#' @export
#' 
read_Neubrandenburg_Data <- function(
  xls.dir, area.Gruendach.m2, area.Kiesdach.m2, pattern = "\\.xlsx$"
)
{
  # Stop here if the directory does not exist
  stopifnot(file.exists(xls.dir))
  
  # Get the full paths to the excel files contained in the selected directory
  xls.files <- dir(xls.dir, pattern = pattern, full.names = TRUE)
  
  # Read all files into one data frame
  allData <- read_Neubrandenburg_Data_from_Excel_files(xls.files)
  
  # Merge data from different files by keeping the same variable from different
  # files in different columns
  validData <- merge_Neubrandenburg_data(allData, keep.all = FALSE)
  
  valid <- is.na(validData$invalid)
  
  if (any(!valid)) {
    
    message("The following rows are invalid (NA) due to differing values!")
    
    print(validData[!valid, ])
  }           
  
  # Convert litres (l) to l/m2
  validData$GD <- validData$KFZ / area.Gruendach.m2
  
  validData$KD <- validData$KFS / area.Kiesdach.m2
  
  validData
}

# read_Neubrandenburg_Data_from_Excel_files ------------------------------------

#' Read Neubrandenburg Data from Excel Files
#' 
#' Read and join Neubrandenburg data from multiple Excel files
#' 
#' @param xls.files vector of (full paths to) MS Excel files containing
#'   Neubrandenburg data
#' 
read_Neubrandenburg_Data_from_Excel_files <- function(xls.files)
{
  contents <- NULL
  
  allTimes <- c()
  
  for (xls in xls.files) {
    
    content <- read_Neubrandenburg_Data_from_Excel(xls)
    
    contents[[basename(xls)]] <- content
    
    allTimes <- kwb.utils::hsRestoreAttributes(
      c(allTimes, content$DateTime), 
      attribs = attributes(content$DateTime)
    )
    
    allTimes <- sort(unique(allTimes))
  }
  
  allData <- data.frame(DateTime = allTimes, stringsAsFactors = FALSE)
  
  numberOfFiles <- length(contents)
  
  for (i in seq_len(numberOfFiles)){
    
    content <-  contents[[i]]
    
    names(content)[-1] <- paste(names(content)[-1], i, sep = ".")
    
    allData <- merge(allData, content, all.x = TRUE)  
  }
  
  structure(allData, source = names(contents))
}

# read_Neubrandenburg_Data_from_Excel ------------------------------------------

#' Read Neubrandenburg Data
#' 
#' Read Neubrandenburg data from Excel file
#' 
#' @param xls full path to MS Excel file containing Neubrandenburg rain gauge
#'   and green roof discharge data
#'   
#' @return data frame with columns \emph{DateTime} (representing Local Normal
#'   Time but with attribute tzone = "UTC"), \emph{NSB}: Niederschlag,
#'   \emph{KFS}: Kiesdach (Kippe, Fallrohr, Stein), \emph{KFZ}: Gruendach
#'   (Kippe, Fallrohr, Zinko)
#'
read_Neubrandenburg_Data_from_Excel <- function(xls) 
{
  # welche Tabellen gibt es?
  tables <- kwb.db::hsTables(xls)
  
  cat("*** Reading", xls, "... ")
  
  rainData <- kwb.db::hsGetTable(
    mdb = xls, tbl = tables[1], fields = "Date, Time, NSB, KFS, KFZ",
    cond = "NOT IsNull(Date)", as.is = TRUE, dbg = FALSE
  )
  
  cat("ok.\n")
  
  # Combine date and ti,me
  rainData$Date <- paste(
    substr(rainData$Date, start = 1, stop = 10), 
    substr(rainData$Time, start = 12, stop = 19)
  )
  
  # Text  in Datum-Uhrzeit umwandeln
  rainData$Date <- as.POSIXct(rainData$Date, tz = "UTC")
  
  # Spalte "Time" entfernen
  rainData <- kwb.utils::removeColumns(rainData, "Time")
  
  # Spalte "Date" umbenennen
  kwb.utils::renameColumns(rainData, list(Date = "DateTime"))
}

# merge_Neubrandenburg_data ----------------------------------------------------

#' Merge Neubrandenburg Data
#' 
#' Merge Neubrandenburg data into one consistent data frame
#' 
#' @param allData data frame
#' @param keep.all logical. If \code{TRUE} (the default is \code{FALSE}) all
#'   columns from \code{allData} are contained in the result data frame, 
#'   otherwise only the first column (timestamp).
#' 
merge_Neubrandenburg_data <- function(allData, keep.all = FALSE)
{
  numberOfFiles <- length(attr(allData, "source"))
  
  numberOfValueColumns <- (ncol(allData) - 1) / numberOfFiles
  
  allChecksPassed <- TRUE
  
  # Start either with all data or just with the time column
  validData <- if (keep.all) {
    
    allData
    
  } else {
    
    allData[, 1, drop = FALSE]
  }
  
  # Append a column "invalid"
  validData$invalid <- NA
  
  for (i in seq_len(numberOfValueColumns)) {
    
    columnNumbers <- seq(
      from = i + 1, by = numberOfValueColumns, length.out = numberOfFiles
    )
    
    columnNames <- names(allData)[columnNumbers]
    
    message(
      "Checking equality of ", kwb.utils::commaCollapsed(columnNames), "... "
    )
    
    # calculate minimum and maximum within each row (= within each column after
    # transposition)
    x <- kwb.utils::colStatistics(
      dataFrame = t(allData[, columnNumbers]), 
      functions = c("min", "max"), 
      na.rm = TRUE
    )
    
    # If the minimum value equals the maximum value all the considered values
    # were equal
    valid <- (x$min == x$max)
    
    # Get the variable name from the first colum name (left of ".<number>")
    variable <- strsplit(columnNames[1], split = "\\.")[[1]][1]
    
    validData[, variable] <- NA
    
    validData[valid, variable] <- x$min[valid]    
    
    # Mark invalid rows with "x"
    validData$invalid[!valid] <- "x"
    
    checkPassed <- all(valid)
    
    cat(ifelse(checkPassed, "ok\n", "differences!\n"))
    
    allChecksPassed <- allChecksPassed && checkPassed
  }  
  
  if (! allChecksPassed) {
    
    warning("There are differences between the values in different files!") 
  }  
  
  validData    
}
