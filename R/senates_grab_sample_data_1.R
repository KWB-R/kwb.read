# PATTERN_STATION --------------------------------------------------------------

#' Pattern Matching a Monitoring Station
#' 
PATTERN_STATION <- function()
{
  structure(
    "^\\s*(Messstelle:\\s*)?(\\d\\d\\d\\D?)\\s+(.*)\\s*$",
    select = c(mpIdName = 1, mpID = 2, mpName = 3)
  )
}

# PATTERN_PARAMETER ------------------------------------------------------------

#' Pattern Matching a Parameter
#' 
PATTERN_PARAMETER <- function()
{
  structure(
    "^((\\d{5})\\s+)?(.*) \\[(.*)\\]",
    select = c(parID = 2, parName = 3, parUnit = 4)
  )
}

# PATTERN_DATA -----------------------------------------------------------------

#' Pattern matching Data
#' 
PATTERN_DATA <- function()
{
  "^\\s*\\d+\\D? \\- \\d+\\s*$"  
}

# hsGetAllGsData1 --------------------------------------------------------------

#' Get All Grab Sample Data (1)
#' 
#' get Senate's grab sample data (type 1) from all csv files in a directory and
#' return it in a "all-in-one-table" data frame
#' 
#' @param csvdir absolute path to directory in which csv files to be read are 
#'   located.
#' @param sep separator in csv file, e.g. ";" or ","
#' @param dateFormat date format specifier describing the format in which dates 
#'   are represented in the csv file. Use placeholders , \code{"\%d"} (day), 
#'   \code{"\%m"} (month), \code{"\%y"} (2-digit year), \code{"\%Y"} (4-digit
#'   year) to describe the date format. \code{"\%d.\%m.\%Y"},
#'   \code{"\%d/\%m/\%y"}, \code{"\%Y-\%m-\%d"} are examples for valid format
#'   specifiers.
#' @param dec decimal character: "." or ","
#' @param data.block.number passed to \code{\link{hsGetGsData1}}, for a 
#'   description see there.
#' @param csvFiles vector of full paths to csv files to be read. All files must
#'   contain data for the same monitoring points, in the same order! Default:
#'   all csv files in \emph{csvdir}
#' 
#' @export
#'   
#' @seealso \code{\link{hsGetGsData1}}
#' 
hsGetAllGsData1 <- function(
  csvdir, sep = ";", dateFormat = "%d.%m.%Y", dec = ",", data.block.number = NA,
  csvFiles = dir(csvdir, "*.csv", full.names = TRUE)
) 
{
  result <- list()
  
  parameterNames.all <- character()
  
  ## Loop through csv files in directory
  for (csv in csvFiles) {  
    
    ## Read data of one monitoring point into structure fileContent
    fileContent <- hsGetGsData1(
      csv = csv, sep = sep, dateFormat = dateFormat, dec = dec, 
      data.block.number = data.block.number
    )
    
    ## Collect parameter names
    parameterNames <- unique(sapply(fileContent$byPar, "[[", "parName"))

    ## Union parameter names with total list of parameter names
    parameterNames.all <- union(parameterNames.all, parameterNames)
    
    result[[fileContent$moniPoint$mpID]] <- fileContent    
  }
  
  structure(result, parNames = parameterNames.all)
}

# hsGetGsData1 -----------------------------------------------------------------

#' Read SENATE's Grab Sample Data from CSV (type 1)
#' 
#' SENATE's grab sample data is read from a csv file (type 1) and returned in
#' forms of a list (see Value section below). The csv file is expected to 
#' contain grab sample data of exactly one monitoring point. For a format 
#' description of the csv file see the Details section below.
#' 
#' The csv file is expected to look like the following example. It is structured
#' into blocks each of which represents a measured parameter. Within each block
#' grab sample results are organized in three columns: 1. sample date, 2.
#' measurement value, 3. character indicating if the detection limit was
#' underrun or exceeded. (see Parameter \code{Sichttiefe} in the example below).
#' The third column is empty if the detection limit was not underrun or exceeded
#' (as in almost all data lines of the example below).
#' 
#' \preformatted{
#' Export Parameterwerte;
#' 
#' 110 Mueggelspree, Faehre Rahnsdorf (0,5 m);
#' 
#' 10001 Lufttemperatur [<degree>C];
#' 
#' ;110 - 10001
#' 06.02.1995;6,000
#' 06.03.1995;5,000
#' ...
#' 03.12.2007;7,000
#' 
#' 10005 Wassertemperatur [<degree>C];
#' 
#' ;110 - 10005
#' 06.02.1995;3,700
#' 06.03.1995;4,600
#' ...
#' 03.12.2007;3,900
#' 
#' 10010 Sichttiefe [cm];
#' 
#' ;110 - 10010
#' 06.02.1995;170,000
#' 06.03.1995;80,000
#' ...
#' 13.11.1995;260,000;>
#' ...
#' }
#' 
#' @param csv full path to csv file
#' @param sep separator in csv file, e.g. ";" or ","
#' @param dateFormat date format specifier describing the format in which dates 
#'   are represented in the csv file. Use placeholders , \code{"\%d"} (day), 
#'   \code{"\%m"} (month), \code{"\%y"} (2-digit year), \code{"\%Y"} (4-digit
#'   year) to describe the date format. \code{"\%d.\%m.\%Y"},
#'   \code{"\%d/\%m/\%y"}, \code{"\%Y-\%m-\%d"} are examples for valid format
#'   specifiers.
#' @param dec decimal character: "." or ","
#' @param data.block.number if the file contains data for more than one 
#'   monitoring point, \emph{data.block.number} needs to be an integer number 
#'   between one and the number of monitoring points for which data are provided
#'   in the file. Only data of monitoring point corresponding to the given 
#'   number are extracted. If \emph{data.block.number} is NA (default) it is 
#'   expected that the file contains data for only one monitoring point. The 
#'   program will stop if data is provided for more than one monitoring point
#' @param dbg if \code{TRUE} (default), debug messages are shown
#' 
#' @return Grab sample data is returned in forms of a list, e.g.:
#'   \preformatted{
#'   $ moniPoint:List of 2
#'   ..$ mpID  : chr "110"
#'   ..$ mpName: chr "M<ue>ggelspree, F<ae>hre Rahnsdorf (0,5 m)"
#'   $ byPar    :List of 104
#'   ..$ 10001  :List of 7
#'   .. ..$ parID  : chr "10001"
#'   .. ..$ parName: chr "Lufttemperatur"
#'   .. ..$ parUnit: chr "<degree>C"
#'   .. ..$ smpDate: Date[1:247], format: "1995-02-06" "1995-03-06" ...
#'   .. ..$ parTxt : chr [1:247] "6,000" "5,000" "12,000" "8,000" ...
#'   .. ..$ parVal : num [1:247] 6 5 12 8 4 5 24 13 16 19 ...
#'   .. ..$ parLim : chr [1:247] "" "" "" "" ...
#'   ..$ 10005  :List of 7
#'   .. ..$ parID  : chr "10005"
#'   .. ..$ parName: chr "Wassertemperatur"
#'   .. ..$ parUnit: chr "<degree>C"
#'   .. ..$ smpDate: Date[1:247], format: "1995-02-06" "1995-03-06" ...
#'   .. ..$ parTxt : chr [1:247] "3,700" "4,600" "7,100" "7,900" ...
#'   .. ..$ parVal : num [1:247] 3.7 4.6 7.1 7.9 13.5 11.3 21.3 15.6 18.2 23.6 ...
#'   .. ..$ parLim : chr [1:247] "" "" "" "" ...
#'   ..$ 10010  :List of 7
#'   .. ..$ parID  : chr "10010"
#'   .. ..$ parName: chr "Sichttiefe"
#'   .. ..$ parUnit: chr "cm"
#'   .. ..$ smpDate: Date[1:238], format: "1995-02-06" "1995-03-06" ...
#'   .. ..$ parTxt : chr [1:238] "170,000" "80,000" "50,000" "110,000" ...
#'   .. ..$ parVal : num [1:238] 170 80 50 110 100 190 140 180 200 200 ...
#'   .. ..$ parLim : chr [1:238] "" "" "" "" ...
#'   ...
#'   }
#' Within the sub-structures representing the parameters, \code{parTxt} contains
#' the values in text format as they were read from the file, \code{parVal}
#' contains the values as they were converted from text to double and
#' \code{parLim} contains the characters indicating underrunning or exceedance
#' of detection limits, as they were extracted from \code{parTxt}.
#' 
#' @seealso \code{\link{hsGetGsData2}, \link{hsGsData1ToList}}
#' 
hsGetGsData1 <- function(
  csv, sep = ";", dateFormat = "%d.%m.%Y", dec = ",", data.block.number = NA,
  dbg = FALSE
) 
{ 
  dataFrame <- .hsReadAndCheck(csv = csv, sep = sep, dbg = dbg)
  
  ## Find line(s) with name of monitoring point
  stationAt <- grep(PATTERN_STATION(), dataFrame$V1)
  
  stationLines <- dataFrame$V1[stationAt]
  
  cat("Monitoring point(s):\n*", paste(stationLines, collapse = "\n* "), "\n")
  
  if (.checkDataBlockNumberOrStop(
    data.block.number, maxNumber = length(stationAt)
  )) {
    
    stationLines <- stationLines[data.block.number]
    
    message("*** Filtering for monitoring point: ", stationLines)
    
    dataFrame <- .extractBlock(
      dataFrame = dataFrame, stationAt = stationAt, 
      data.block.number = data.block.number
    )
  }  
  
  # @TODO: separation between extraction
  #        and conversion
  #        -> formats specs not here!
  result <- .extractParameterBlocks(
    dataFrame, sep = sep, dec = dec, dateFormat = dateFormat,
    mpInfo = .hsMoniPointInfo(stationLines, PATTERN_STATION(), dbg = dbg),
    dbg = dbg
  )
  
  result
}

# .hsReadAndCheck --------------------------------------------------------------

.hsReadAndCheck <- function(csv, sep, dbg = FALSE)
{
  # Read the CSV file into a data frame
  dataFrame <- .readGrabSampleCsvFile(csv = csv, sep = sep)
  
  # Stop if there are too few rows or too few columns
  .stopOnTooFewRowsOrColumns(dataFrame = dataFrame, dbg = dbg)
  
  # If there is no column "V3" (below or above detection limit) add it
  dataFrame <- kwb.utils::hsAddMissingCols(dataFrame, "V3", fill.value = "")
  
  # Remove empty lines
  .removeEmptyRows(dataFrame = dataFrame, dbg = dbg)  
}

# .readGrabSampleCsvFile -------------------------------------------------------

.readGrabSampleCsvFile <- function(
  csv, sep, ncol = .getMaxNumberOfFields(csv = csv, sep = sep, quote = quote), 
  quote = ""
)
{
  # Read csv file into data.frame  
  cat("*** Reading", csv, "...")
  
  dataFrame <- utils::read.csv(
    file = csv, header = FALSE, sep = sep, blank.lines.skip = TRUE,  
    as.is = TRUE, fill = TRUE, col.names = sprintf("V%d", seq_len(ncol))
  )
  
  cat("ok.\n")
  
  dataFrame
}

# .getMaxNumberOfFields --------------------------------------------------------

.getMaxNumberOfFields <- function(csv, sep, quote)
{
  # Get maximum number of fields in CSV file
  numberOfFields <- utils::count.fields(csv, sep = sep, quote = quote)
  
  isNA <- is.na(numberOfFields)
  
  if (any(isNA)) {    
    
    stop(
      "There are quoted fields containing new line, starting with:\n",
      kwb.utils::lastElement(readLines(csv, n = which(isNA)[1])), 
      call. = FALSE
    )
  }
  
  max(numberOfFields, na.rm = TRUE)  
}

# .stopOnTooFewRowsOrColumns ---------------------------------------------------

.stopOnTooFewRowsOrColumns <- function(dataFrame, dbg)
{
  kwb.utils::catIf(dbg, sprintf(
    "The CSV file contains a matrix of %d rows and %d columns.\n",
    nrow(dataFrame), ncol(dataFrame)
  ))
  
  if (nrow(dataFrame) < 3) {
    
    stop("Too few rows.\n")
  }
  
  if (ncol(dataFrame) < 2) {
    
    stop("Too few columns.\n")
  }  
}

# .removeEmptyRows -------------------------------------------------------------

.removeEmptyRows <- function(dataFrame, dbg = FALSE)
{
  isEmpty <- rowSums(! kwb.utils::isNaOrEmpty(as.matrix(dataFrame))) == 0
  
  kwb.utils::catIf(dbg, sprintf("Removing %d empty lines... ", sum(isEmpty)))  
  
  dataFrame <- dataFrame[! isEmpty, ]
  
  kwb.utils::catIf(dbg, "ok.\n")
  
  dataFrame
}

# .checkDataBlockNumberOrStop --------------------------------------------------

.checkDataBlockNumberOrStop <- function(data.block.number, maxNumber)
{
  if (is.na(data.block.number) && maxNumber > 1) {
    
    stop(
      "More than one monitoring point found in file! Use data.block.number ",
      "to select the monitoring point of which data are to be extracted.",
      call. = FALSE
    ) 
  } 
  
  if (! is.na(data.block.number) && 
      ! kwb.utils::inRange(data.block.number, 1, maxNumber)) 
  {
    stop(
      "data.block.number must be an integer number between 1 and ", maxNumber, 
      call. = FALSE
    )
  }
  
  return (! is.na(data.block.number))
}

# .extractBlock ----------------------------------------------------------------

.extractBlock <- function(dataFrame, stationAt, data.block.number)
{
  # Last row is row before first row of previous block or nrow(dataFrame) at max
  to <- if (data.block.number < length(stationAt)) {
    
    stationAt[data.block.number + 1] - 1
    
  } else {
    
    nrow(dataFrame)
  }

  dataFrame[seq(from = stationAt[data.block.number], to = to), ]  
}

# .extractParameterBlocks ------------------------------------------------------

.extractParameterBlocks <- function(
  dataFrame, sep, dec, dateFormat, mpInfo, dbg
)
{
  ## Find rows containing parameter info (id, name, unit)
  parameterLinesAt <- grep(PATTERN_PARAMETER(), dataFrame$V1, value = FALSE)
  
  ## Find headers of data blocks
  dataHeadersAt <- grep(PATTERN_DATA(), dataFrame$V2, value = FALSE)
  
  # Check if header lines were found at expected positions or stop
  .checkHeaderLinesOrStop(dataHeadersAt, parameterLinesAt, sep = sep, dbg = dbg)
  
  ## Prepare result list
  result <- list()
  
  ## Save monitoring point info in result object
  result[["moniPoint"]] <- list(mpID = mpInfo$mpID, mpName = mpInfo$mpName)  
  
  ## Split lines containing parameter names into parID, parName, parUnit
  parameterLines <- dataFrame$V1[parameterLinesAt]
  
  kwb.utils::catIf(dbg, "Parameters:\n*", paste(parameterLines, collapse = "\n* "), "\n")
  
  parInfo <- .hsParInfo(parameterLines, PATTERN_PARAMETER(), dbg = dbg)
  
  ## Are the parameter IDs unique? Show duplicates and repair them!
  parInfo <- .hsCheckParInfo(parInfo, repair = TRUE)
  
  cat("Checking repaired parInfo...\n")
  
  .hsCheckParInfo(parInfo, repair = FALSE)
  
  ## block begin and block end rows
  blockBeginAt <- dataHeadersAt + 1
  
  blockEndAt <- c((parameterLinesAt - 1)[-1], nrow(dataFrame))  
  
  ## Loop through data blocks
  indices <- seq_len(length(blockBeginAt))
  
  for (i in indices) {    
    
    kwb.utils::catIf(dbg, sprintf(
      "*** Parameter %d/%d: %s\n", i, length(indices), parameterLines[i]
    ))
    
    parameterInfo <- parInfo[i, ]
    
    dataBlock <- .createDataBlockForParameter(
      data = dataFrame[blockBeginAt[i]:blockEndAt[i], ], # cut data block
      parameterInfo = parameterInfo, dateFormat = dateFormat, dec = dec
    )
    
    ## Append parameter block to result list
    ## generate a non-existing key from the parameter ID by adding 
    ## ".1", ".2", ".3", ... if necessary
    key <- .getUniqueKey(parameterInfo$parID, existing = names(result$byPar))
    
    result$byPar[[key]] <- dataBlock
  }  
  
  result
}

# .checkHeaderLinesOrStop ------------------------------------------------------

.checkHeaderLinesOrStop <- function(dataHeadersAt, parameterLinesAt, sep, dbg)
{
  if (length(dataHeadersAt) < 1) {
    
    stop(sprintf(paste(
      "Too few data blocks found. Did you use the correct",
      "column separator (\"%s\")?", sep = sep
    )))
  }
  
  ## Test: block header must be always one line after parameter line ids
  if (any(dataHeadersAt - parameterLinesAt != 1)) {
    
    stop("block header is not always one line below parameter describing line!")    
    
  } else {
    
    kwb.utils::catIf(dbg, "Check of block header lines ok.\n")
  }
}

# .createDataBlockForParameter -------------------------------------------------

.createDataBlockForParameter <- function(data, parameterInfo, dateFormat, dec)
{
  kwb.utils::checkForMissingColumns(data, paste0("V", 1:3))
  
  kwb.utils::checkForMissingColumns(
    parameterInfo, c("parID", "parName", "parUnit")
  )
  
  list(
    parID = parameterInfo$parID,
    parName = parameterInfo$parName, 
    parUnit = parameterInfo$parUnit,
    ## Convert strings to date and stop on failure
    smpDate = kwb.utils::hsStringToDate(data$V1, dateFormat),   
    parTxt  = data$V2,
    ## Convert string to double
    parVal = kwb.utils::hsStringToDouble(data$V2, dec),
    parLim  = data$V3
  )
}

# .getUniqueKey ----------------------------------------------------------------

.getUniqueKey <- function(x, existing)
{
  pattern <- paste0("^", x, "(\\.\\d+)?$")
  
  numberOfExisting <- length(grep(pattern, existing))
  
  if (numberOfExisting > 0) {
    
    x <- sprintf("%s.%d", x, numberOfExisting + 1)
  }      
  
  x
}

# .hsMoniPointInfo -------------------------------------------------------------

#' Extract mpIdName, mpID, mpName from String
#' 
#' Extract mpIdName, mpID, mpName from monitoring point string
#' 
.hsMoniPointInfo <- function(
  stationLines, pattern = PATTERN_STATION(), dbg = FALSE
) 
{
  kwb.utils::printIf(dbg, stationLines, "Extracting moni point info from")
  
  result <- .extractSubExpressionsAndRowBind(pattern, stationLines)
  
  result$mpIdName <- paste(result$mpID, result$mpName, sep = ": ")
  
  kwb.utils::printIf(dbg, result, "extracting monitoring point info ok")
  
  result
}

# .hsParInfo -------------------------------------------------------------------

#' Extract parID, parName, parUnit from Parameter String
#' 
.hsParInfo <- function(parLine, pattern = PATTERN_PARAMETER(), dbg = FALSE)
{     
  kwb.utils::printIf(dbg, parLine, "Extracting par info from")

  result <- .extractSubExpressionsAndRowBind(pattern, parLine)
  
  kwb.utils::printIf(dbg, result, "extracting parameter info ok")
  
  result
}

# .extractSubExpressionsAndRowBind ---------------------------------------------

.extractSubExpressionsAndRowBind <- function(pattern, text)
{
  matches <- kwb.utils::subExpressionMatches(
    regularExpression = pattern, text = text, select = attr(pattern, "select"),
    simplify = FALSE
  )
  
  kwb.utils::rbindAll(lapply(matches, data.frame, stringsAsFactors = FALSE))
}

# .hsCheckParInfo --------------------------------------------------------------

#' Check Parameter Information for Ambiguities
#' 
.hsCheckParInfo <- function(parInfo, repair = TRUE, dbg = FALSE)
{
  ## Find and show duplicates (parameters with the same ID)
  isDuplicated <- duplicated(parInfo$parID)
  
  if (any(isDuplicated)) {
    
    kwb.utils::printIf(
      dbg, parInfo[parInfo$parID %in% parInfo$parID[isDuplicated], ], 
      "*** Parameters with duplicate IDs"
    )
    
  } else {
    
    cat("*** Parameter IDs are unique.\n")      
  }
  
  ## Are there duplicate combinations of parID, parName and parUnit? 
  isDuplicated <- duplicated(parInfo)
    
  if (any(isDuplicated)) {
    
    duplicates <- parInfo[isDuplicated, ]
    
    kwb.utils::printIf(
      TRUE, duplicates, 
      "*** Parameters with duplicate combination of id, name, unit"
    )
    
    ## Repair if desired
    if (repair) {
      
      parInfo <- .repairParInfo(parInfo, duplicates)
      
    } else {
      
      cat("I will not repair.\n")
    }
    
  } else {
    
    cat("*** No parameters with duplicate combination of id, name, unit.\n")    
  }
  
  ## Return parInfo
  parInfo
}

# .repairParInfo ---------------------------------------------------------------

.repairParInfo <- function(parInfo, duplicates)
{
  # Append row number to data frames parInfo and duplicates and merge
  duplicateInfo <- merge(
    cbind(duplicates, row.dup = seq_len(nrow(duplicates))),
    cbind(parInfo, row.par = seq_len(nrow(parInfo)))
  )
  
  # Order by index in parInfo (-> append "_1", "_2" as it was done before)
  duplicateInfo <- duplicateInfo[order(duplicateInfo$row.par), ]
  
  ## Loop through indices representing duplicate combinations
  for (row.dup in unique(duplicateInfo$row.dup)) {
    
    ## Find the corresponding row indices in parInfo
    indices.par <- duplicateInfo$row.par[duplicateInfo$row.dup == row.dup]
    
    kwb.utils::printIf(TRUE, parInfo[indices.par, ], "parInfo to repair")
    
    # Append "_1", "_2", ... to the parameter name
    parInfo$parName[indices.par] <- paste(
      parInfo$parName[indices.par], 
      seq_len(length(indices.par)), 
      sep = "_"
    )

    kwb.utils::printIf(TRUE, parInfo[indices.par, ], "Repaired parInfo")
  }
  
  parInfo
}

# hsGsData1ToList --------------------------------------------------------------

#' Transform Grab Sample Data (Type 1)
#' 
#' Transform grab sample data (type 1) as returned by \code{\link{hsGetGsData1}}
#' to "all-in-one-table"-format
#' 
#' @param gsData1 grab sample data structure as returned by
#'   \code{\link{hsGetGsData1}}
#' @param namesAsFactor passed to \code{\link[kwb.utils]{rbindAll}}
#' @param dbg if \code{TRUE} (default), debug messages are shown
#' 
#' @export
#' 
#' @seealso \code{\link{hsGetGsData1}, \link{hsGsData2ToList}}
#'   
hsGsData1ToList <- function(gsData1, namesAsFactor = FALSE, dbg = TRUE) 
{  
  kwb.utils::catIf(dbg, "Converting list structure to data frame in 'long' format... ")
  
  result <- kwb.utils::rbindAll(
    x = lapply(gsData1, function(stationData) {
      
      kwb.utils::rbindAll(
        x = lapply(stationData$byPar, function(parameterData) {
          
          kwb.utils::removeColumns(
            dframe = as.data.frame(parameterData, stringsAsFactors = FALSE), 
            columnsToRemove = c("parID", "parTxt")
          )
        }), 
        nameColumn = "parID", namesAsFactor = namesAsFactor
      )
    }),
    nameColumn = "mpID", namesAsFactor = namesAsFactor
  )
  
  kwb.utils::catIf(dbg, "ok.\n")
  
  firstColumns <- c("mpID", "parID")
  
  result[, c(firstColumns, setdiff(names(result), firstColumns))]
}
