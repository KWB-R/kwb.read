# hsGetGsData2 -----------------------------------------------------------------

#' Read SENATE's Grab Sample Data From CSV (type 2)
#' 
#' SENATE's grab sample data is read from a csv file (type 2) and returned in
#' forms of a list (see Value section below). The csv file is expected to
#' contain grab sample data of more than one monitoring point, organized in
#' sections each of which contains measurement results of all available 
#' parameters in a matrix-style format. For a format description of the csv file
#' see the Details section below.
#' 
#' The csv file is expected to look like the following example. It is structured
#' into blocks each of which represents a monitoring point. Such, it is
#' different from the format supported by \code{\link{hsGetGsData1}} where each
#' block represents a measured parameter and there is only one monitoring point
#' per file. Here, a block starts with a line naming the monitoring point,
#' followed by a header line showing the names and units of the analysed
#' parameters. The header line is followed by data lines each of which
#' represents a sample with sample data and the measured values according to the
#' names in the header line. The value fields may contain a leading character
#' indicating that the detection limit was underrun or exceeded. (Shortened)
#' example of an input file:
#' \preformatted{
#' G<ue>te - Parameterwerte (Tabelle 3);;; ...
#' ;;; ...
#' Messstelle: 110 M<ue>ggelspree, F<ae>hre Rahnsdorf M<ue>ggelspree;;; ...
#' ;TL, gesamt [<degree>C];TW, gesamt [<degree>C];Sicht, gesamt [cm];pH, gesamt [Wert]; ...
#' 13.01.2011;3,000;0,400;;7,200;647,000;7,500;51,826;36,000;< 0,050; ...
#' 10.02.2011;1,000;3,100;;7,600;621,000;11,300;84,231;35,000;< 0,050; ...
#' ...
#' ;;; ...
#' Messstelle: 130 Spree, F<ae>hre Baumschulenweg Spree;;; ...
#' ;TL, gesamt [<degree>C];TW, gesamt [<degree>C];Sicht, gesamt [cm];pH, gesamt [Wert]; ...
#' 02.11.2010;7,000;7,400;120,000;8,000;616,000;9,000;75,069;37,000; ...
#' 30.11.2010;-4,000;2,700;150,000;7,900;608,000;10,100;74,464;36,000; ...
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
#' @param blockBeginPtrn pattern indicating the begin of a data block in the 
#'   file
#' @param dbg if \code{TRUE} (default), debug messages are shown
#' @param outlevel Output level. Expected values are 1 or 2. The default is 1.
#'   If the output level is 2, the raw data block is passed to 
#'   \code{kwb.read:::.hsExtractData} before being returned.
#'   
#' @return Grab sample data is returned in forms of a list, e.g.:
#'   \preformatted{
#'   $ moniPoint:List of 2
#'   ..$ mpID  : chr , e.g. "730"
#'   ..$ mpName: chr , e.g. "Panke, Muendung (Nordhafen-Vorbecken) (0,5 m)"
#'   $ byPar    :List 
#'   ..$ <parID1>:List of 7
#'   .. ..$ parID  : chr , e.g. "10001"
#'   .. ..$ parName: chr , e.g. "Lufttemperatur"
#'   .. ..$ parUnit: chr , e.g. "<degree>C"
#'   .. ..$ smpDate: Date[], format: ...
#'   .. ..$ parTxt : chr [], e.g. "3,000" "4,000" "9,300" "19,900" ...
#'   .. ..$ parVal : num [], e.g. 3 4 9.3 19.9 12.1 13.1 19.6 28 13.9 7.2 ...
#'   .. ..$ parLim : chr [], e.g. "" "<" "<" "" ...
#'   ..$ <parID2> ...
#'   }  
#' 
#' @export
#' 
#' @seealso \code{\link{hsGetGsData1}, \link{hsGsData2ToList}}
#' 
hsGetGsData2 <- function(
  csv, sep = ";", dateFormat = "%d.%m.%Y", dec = ",", 
  blockBeginPtrn = PATTERN_STATION(), dbg = FALSE, outlevel = 1
) 
{ 
  ## Read data from csv into data frame, check for least rows/columns
  ## and remove empty lines
  dataFrame <- .hsReadAndCheck(csv, sep)
  
  ## Row indices of "block begins", each block representing a monitoring point
  monitoringPointAt <- grep(blockBeginPtrn, dataFrame[[1]])
    
  # Extract information on monitoring points (mpIdName, mpID, mpName)
  mpInfo <- .getMonitoringPointInfoOrStop(
    stationLines = dataFrame[monitoringPointAt, 1],
    blockBeginPtrn = blockBeginPtrn
  )
  
  blockRanges <- kwb.utils::startsToRanges(
    starts = monitoringPointAt, lastStop = nrow(dataFrame), startOffset = 1,
    stopOffset = 1
  )
  
  ## Prepare result list
  result <- list()
  
  # Loop through blocks
  for (i in seq_len(nrow(blockRanges))) {
    
    cat(sprintf("=== Monitoring point: %s ===\n", mpInfo$mpIdName[i]))
    
    # Get the "raw" data block
    rows <- seq(blockRanges$from[i], blockRanges$to[i])
    
    columns <- seq_len(.get_ncol(headerRow = dataFrame[blockRanges$from[i], ]))

    kwb.utils::catIf(dbg, "Number of columns in this block:", length(columns), "\n")
    
    data <- dataFrame[rows, columns]
    
    ## Replace raw data block with extracted data block if this level of output
    ## is requested
    if (outlevel == 2) {
      
      data <- .hsExtractData(data = data, dateFormat = dateFormat, dec = dec)
    }
    
    ## Create result list
    result[[mpInfo$mpID[i]]] <- list(
      moniPoint = list(mpID = mpInfo$mpID[i], mpName = mpInfo$mpName[i]), 
      allPars = data
    )
  }
  
  result
}

# .getMonitoringPointInfoOrStop ------------------------------------------------

.getMonitoringPointInfoOrStop <- function(stationLines, blockBeginPtrn)
{
  if (length(stationLines) < 1) {
    
    stop(call. = FALSE, sprintf(
      "Too few data blocks (starting with \"%s\").\n", blockBeginPtrn
    ))
  }
  
  ## Extract line on monitoring point into its parts
  mpInfo <- .hsMoniPointInfo(
    stationLines = stationLines, pattern = blockBeginPtrn
  )
  
  cat(sprintf(
    "%d monitoring points identified:\n%s\n", length(stationLines),
    paste(mpInfo$mpIdName, collapse = "\n")
  ))
  
  ## Stop if names of monitoring points are not unique
  if (length(unique(mpInfo$mpName)) != length(mpInfo$mpName)) {
    
    stop(
      "Names of moinitoring points are ambiguous. ",
      "Did you use the wrong column separator?", call. = FALSE
    )
  }
  
  mpInfo
}

# .hsExtractData ---------------------------------------------------------------

#' Convert Data Types in Data Block
#' @keywords internal
#' @noRd
.hsExtractData <- function(data, dateFormat, dec) 
{
  ## Parameter values, possibly with comma "," and starting with "<" or ">"
  parValTxt1 <- as.matrix(data[-1, -1])
  
  parValPtrn <- "^\\s*([<>]?)\\s*(.*)$"
  
  parValTxt2 <- sub(parValPtrn, "\\2", parValTxt1)
  
  parVal <- matrix(
    kwb.utils::hsStringToDouble(parValTxt2, dec), nrow = nrow(parValTxt2)
  )
  
  rownames(parVal) <- rownames(parValTxt2)
  
  colnames(parVal) <- colnames(parValTxt2)
  
  ## Parameter name including unit
  parNameUnit <- as.character(data[1, -1])
  
  parNamePtrn <- "^\\s*(.*)\\s+(\\[(.*)\\])\\s*$"
  
  parInfo <- .hsParInfo(parNameUnit)
  
  ## Convert string to date and stop on failure
  smpDate <- kwb.utils::hsStringToDate(data$V1[-1], dateFormat = dateFormat) 
  
  result <- list(
    parID   = parInfo$parID,
    parName = parInfo$parName,
    parUnit = parInfo$parUnit,
    smpDate = smpDate,
    parTxt  = parValTxt1,
    parVal  = parVal,
    parLim  = sub(parValPtrn, "\\1", parValTxt1)
  )
  
  result
}

# hsGsData2ToList --------------------------------------------------------------

#' Grab Sample Data 2 to List
#' 
#' Transform grab sample data (type 2) as returned by \code{\link{hsGetGsData2}}
#' to "all-in-one-table"-format
#' 
#' @param gsData2 grab sample data structure as returned by
#'   \code{\link{hsGetGsData2}}
#' @param dbg if \code{TRUE} (default), debug messages are shown
#'   
#' @seealso \code{\link{hsGetGsData2}, \link{hsGsData1ToList}}
#' 
hsGsData2ToList <- function(gsData2, dbg = FALSE) 
{
  ## allPar to list representation
  result <- NULL
  
  for (mpID in names(gsData2)) {
    
    allPar <- gsData2[[mpID]]$allPars
    
    for (i in seq_along(allPar$parID)) {
      
      kwb.utils::catIf(dbg, sprintf(
        "mpID: %s, parNr. %d/%d\n", mpID, i, length(allPar$parID)
      ))
      
      block <- data.frame(
        mpID    = mpID,
        parID   = allPar$parID[i],
        parName = allPar$parName[i],
        parUnit = allPar$parUnit[i],
        smpDate = allPar$smpDate,
        parVal  = allPar$parVal[, i],
        parLim  = allPar$parLim[, i],
        stringsAsFactors = FALSE
      )
      
      result <- rbind(result, block)
    }    
  }
  
  result
}
