# read_qin_file ----------------------------------------------------------------

#' Read "qin"-File
#' 
#' Read data, extract starttime, timestep, manhole IDs and runoff data
#' 
#' @param infile full path to input file.
#' @param columnWidth width of the data columns. It is assumed that all data
#'   columns have the same width!
#' @param n number of (data) rows to read. Set to -1 if all rows are to be read.
#'   Default: -1
#' @param headerSize number of header rows preceeding the data rows in
#'   \emph{infile}
#' @param dbg if TRUE, debug messages are shown, else not
#' 
#' @export
#' 
read_qin_file <- function(
  infile, columnWidth = 8, n = -1, headerSize = 528, dbg = TRUE
) 
{
  # Read header lines plus one data line (to determine number of columns)
  kwb.utils::catIf(dbg, "*** Reading the file header... ")
  
  fileheaderPlus1 <- readLines(infile, n = headerSize + 1)
  
  kwb.utils::catIf(dbg, "ok.\n")
  
  # extract time information from line 2
  kwb.utils::catIf(dbg, "*** Extracting header information...\n")
  
  timeline <- fileheaderPlus1[2]
  
  startdate.txt <- substr(timeline, start = 7, stop = 18)
  
  timestep <- as.numeric(substr(timeline, start = 21, stop = 23))
  
  startdate <- as.POSIXct(startdate.txt, format = "%d%m%Y%H%M")
  
  kwb.utils::catIf(dbg, "  * startdate (character):", startdate.txt, "\n")
  kwb.utils::catIf(dbg, "  * startdate (POSIXct):", format(startdate), "\n")
  kwb.utils::catIf(dbg, "  * timestep:", timestep, "\n")
  
  # extract manhole IDs from all but the first two and the last (being the first 
  # data line) header lines
  exclude <- c(1, 2, headerSize + 1)
  
  manholeIds <- substr(fileheaderPlus1[-exclude], start = 12, stop = 20)
  
  kwb.utils::catIf(dbg, "*** Extracting header information ok.\n")
  
  # Determine number of columns of 'columnWidth' characters width each
  ncol <- nchar(kwb.utils::lastElement(fileheaderPlus1)) / columnWidth
  
  # The manhole-IDs (with the last excluded, why?) will be used as column names
  columnNames <- manholeIds[-length(manholeIds)]
  
  message(
    "\nHey Mathias, why are there ", length(manholeIds), 
    " manholeIds but only ", ncol, " data columns?\n"
  )
  
  # Now read in the complete file
  kwb.utils::catIf(dbg, "*** Reading the complete file... ")
  
  runoff_raw <- readLines(infile, n = ifelse(n == -1, -1, headerSize + n))
  
  kwb.utils::catIf(dbg, "ok.\n")
  
  # exclude the header lines
  kwb.utils::catIf(dbg, "*** Exclucing the header lines... ")
  
  runoff_raw <- runoff_raw[-seq_len(headerSize)]
  
  kwb.utils::catIf(dbg, "ok.\n")
  
  # validate the text lines
  kwb.utils::catIf(dbg, "*** Validating the data lines... ")
  
  .validateRowLengths(runoff_raw)
  
  kwb.utils::catIf(dbg, "ok.\n")
  
  # Cut the textblock into columns and convert to numeric
  kwb.utils::catIf(
    dbg, "*** Splitting data into columns and converting to numeric...\n"
  )
  
  runoff <- extractRunoffData(runoff_raw, columnNames = columnNames)
  
  kwb.utils::catIf(
    dbg, "*** Splitting data into columns and converting to numeric ok.\n"
  )
  
  # add timestamp column
  timestamp <- seq(startdate, by = timestep, length.out = nrow(runoff))
  
  cbind(timestamp, runoff)    
}

# .validateRowLengths ----------------------------------------------------------

.validateRowLengths <- function(runoff_raw)
{
  rowlengths <- nchar(runoff_raw)
  
  uniqueLengths <- unique(rowlengths)
  
  if (length(uniqueLengths) > 1) {
    
    frequencies <- sapply(uniqueLengths, function(n) sum(rowlengths == n))
    
    frequencyOrder <- order(frequencies, decreasing = TRUE)
    
    warning(
      sprintf(
        paste(
          "There are differing row lengths in the data block.",
          "The most frequent row length is %d.\n ",
          "There are %d rows which have differing lengths:\n   %s."
        ), 
        uniqueLengths[frequencyOrder][1],
        sum(frequencies[frequencyOrder][-1]),
        paste0(
          frequencies[frequencyOrder][-1], "-times ", 
          uniqueLengths[frequencyOrder][-1], " chars",
          collapse = ",\n    "
        )
      )
    )
  }
}

# extractRunoffData ------------------------------------------------------------

#' Separation of Fixed Width Columns
#' 
#' Separation of columns of 'columnWidth' characters width each
#' 
#' @param runoff_raw vector of character representing the text lines containing
#'   data in columns of fixed width
#' @param columnWidth number of characters in each column
#' @param columnNames column names to be given to the result data frame
#' @param version 1: loop, 2: sapply
#' 
#' @return The function returns a data frame with column names as given in 
#'   \code{columnNames}.
#' 
extractRunoffData <- function(
  runoff_raw, columnWidth = 8, columnNames = NULL, version = 1
)
{
  # number of values per line, each value consists of columnWidth characters
  n <- nchar(runoff_raw[1]) / columnWidth 
  
  extractAndConvertColumn <- function(i) {
    
    as.numeric(substr(runoff_raw, (i-1) * columnWidth + 1, i * columnWidth))
  }
  
  stats::setNames(
    object = as.data.frame(sapply(seq_len(n), FUN = extractAndConvertColumn)), 
    nm = columnNames
  )
}
