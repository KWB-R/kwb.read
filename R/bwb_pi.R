# getPiColumnRenames -----------------------------------------------------------

#' Get List of Renamings for PI-Identifiers
#' 
#' Get a list of assignements mapping PI identifiers to "real" (short) names
#' 
#' @param mdb path to MS Access Database Containing Meta Data
#' 
#' @return list of key = value pairs with the keys representing the
#'   PI-identifiers and the values representing the real (short) names
#'
#' @export
#' 
getPiColumnRenames <- function(mdb = mdb_rain_meta())
{
  # Read table mapping PI identifier to description from a database that was 
  # created in the MIA-CSO project
  info <- kwb.db::hsGetTable(
    mdb,
    tbl = "Regenschreiber_info", 
    fields = "PI_Nr, Beschreibung",
    stringsAsFactors = FALSE,
    dbg = FALSE
  )
  
  kwb.utils::toLookupList(
    keys = gsub("\\.C1$", "", kwb.utils::selectColumns(info, "PI_Nr")),
    values = kwb.utils::substSpecialChars(kwb.utils::multiSubstitute(
      kwb.utils::selectColumns(info, "Beschreibung"),
      replacements = list("^\\d\\d\\.\\d\\d\\s+|\\s+- Regen pro Tag" = "")
    ))
  )
}

# get_PI_data_Halensee ---------------------------------------------------------

#' Get PI Data Halensee
#' 
#' @param xls.file full path to MS Excel file containing PI data of Halensee
#' @param sheetName name of the cell range or worksheet to read. Default:
#'   "mydata". There should be a cell range named "mydata" in the Excel file.
#'   Create this named cell range by 1. selecting one non-empty cell within the
#'   actual body of the table, 2. pressing Strg+A (select all), 3. writing
#'   "mydata" into the name field (white input field, just above the cell area
#'   and left of the "edit bar" labelled "fx"), and (iv) pressing Enter.
#'
#' @export
#' 
get_PI_data_Halensee <- function(xls.file, sheetName = "mydata")
{
  hint <- paste(
    "Create a named cell range in Excel and set sheetName to the name of that ",
    "cell range (default: 'mydata'). See ?kwb.read::get_PI_data_Halensee"
  )
  
  if (! (sheetName %in% kwb.db::hsTables(xls.file))) {
    
    stop(sprintf(
      "There is no cell range or worksheet named '%s'!\n%s", 
      sheetName, hint
    ))
  }
  
  rawdata <- kwb.db::hsGetTable(
    xls.file, sheetName, as.is = TRUE, stringsAsFactors = FALSE
  )

  # Stop if the header line was not recognised (all column names are starting
  # with "F" and ending with a number)
  if (all(grepl("^F\\d+$", names(rawdata)))) {
    
    stop(paste("I could not identify the column names.", hint ))
  }
  
  mydata <- kwb.utils::hsDelEmptyCols(rawdata)
  
  mydata <- mydata[! is.na(kwb.utils::selectColumns(mydata, "F1")), ]
  
  variables <- readPackageFile("PI_Data_Halensee_Variables.csv")
  
  renames <- kwb.utils::toLookupList(
    keys = kwb.utils::selectColumns(variables, "PI_ID"), 
    values = paste(
      kwb.utils::selectColumns(variables, "VariableCode"), 
      kwb.utils::selectColumns(variables, "SiteCode"), 
      sep = "_"
    )
  )
  
  mydata <- kwb.utils::renameColumns(mydata, c(renames, F1 = "DateTime"))
  
  datetimes <- kwb.utils::selectColumns(mydata, "DateTime")
  datetimes <- kwb.datetime::hsToPosix(datetimes)
  datetimes <- kwb.base::hsST2WT(datetimes)
  
  mydata$DateTime <- datetimes
  
  kwb.utils::renameColumns(mydata, list(DateTime = "DateTimeUTCplus1"))
}

# readPackageFile --------------------------------------------------------------

#' Read CSV File from Package's "extdata" Folder
#' 
#' @param file file name (without path)
#' @param \dots additional arguments passed to \code{\link[utils]{read.csv}}
#' 
#' @return data frame representing the content of \code{\link{file}}
#' 
#' @export
#' 
readPackageFile <- function(file, ...)
{
  kwb.utils::readPackageFile(file, package = "kwb.read", sep = ";", ...)
}
