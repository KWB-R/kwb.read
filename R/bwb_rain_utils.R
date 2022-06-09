# .mergeGaugeLists -------------------------------------------------------------

.mergeGaugeLists <- function(dataList, strict = TRUE)
{
  FUN <- ifelse(strict, kwb.utils::getAttribute, attr)
  
  gaugeList <- lapply(dataList, FUN, "gauges")
  
  gauges <- unique(do.call(rbind, gaugeList))
  
  gauges <- gauges[order(kwb.utils::selectColumns(gauges, "ObjNr")), ]
  
  kwb.utils::resetRowNames(gauges)
}

# .mergeMeansLists -------------------------------------------------------------

.mergeMeansLists <- function(dataList)
{
  meansList <- lapply(dataList, kwb.utils::getAttribute, "means")
  
  means <- kwb.utils::safeRowBindAll(meansList)
  
  means <- means[order(means[, 1]), ]
  
  kwb.utils::resetRowNames(means)  
}

# .mergeSumsLists --------------------------------------------------------------

.mergeSumsLists <- function(dataList)
{
  sumsList <- lapply(dataList, kwb.utils::getAttribute, "sums")
  
  kwb.utils::safeRowBindAll(sumsList)
}

# .stopOnTooFewColumns ---------------------------------------------------------

.stopOnTooFewColumns <- function(data, expected, n.rows = 8)
{
  if (ncol(data) < expected) {
    
    message("The first rows look like this ('<<<' indicates start of line):")
    
    unlistAsCharacter <- function(x) as.character(unlist(x))
    
    textlines <- apply(utils::head(data, n = n.rows), 1, unlistAsCharacter)
    
    kwb.utils::catLines(paste0(">>>", textlines))
    
    stop(
      "At least ", expected, " non-empty columns expected. ", 
      .text.wrong.format(), .text.wrong.sep(), call. = FALSE
    )
  }
}

# .stopOnTooFewFiles -----------------------------------------------------------

.stopOnTooFewFiles <- function(files)
{
  if (! length(files)) {
    
    stop("At least one file must be given!")
  }
}

# .text.wrong.format -----------------------------------------------------------

.text.wrong.format <- function() 
{
  "The file does not seem to be in the right format. "
}

# .text.wrong.sep --------------------------------------------------------------

.text.wrong.sep <- function() 
{
  "Did you specify the correct column separator in 'sep'?"
}

# .localeString ----------------------------------------------------------------

.localeString <- function(country)
{
  locales <- list(
    de = c("de_DE", "German"),
    en = c("en_EN", "English")
  )
  
  locale <- locales[[country]]
  
  if (is.null(locale)) {
    
    stop(
      "No locale string available for country ", 
      kwb.utils::hsQuoteChr(country), ". Available country strings: ",
      kwb.utils::stringList(names(locales)), call. = FALSE
    )
  }
  
  locale
}

# .convertToDateStrings --------------------------------------------------------

.convertToDateStrings <- function(
  datestrings, format, prefix = "", locale = .localeString("de"), dbg = TRUE
)
{
  #prefix="";dbg=TRUE
  out <- if (all(grepl("^\\d+$", datestrings))) {
    
    kwb.utils::catIf(dbg, "All datestrings are integer values.\n")
    
    .numberToDatestring(as.integer(datestrings), dbg)
    
  } else {
    
    .localeToDatestring(datestrings, format, prefix, locale)
  }
  
  isNA <- is.na(out)
  
  if (any(isNA)) {
    
    stop(
      "Could not convert these datestrings to dates: ", 
      kwb.utils::stringList(datestrings[isNA]), call. = FALSE
    )
  }
  
  out
}

# .numberToDatestring ----------------------------------------------------------

.numberToDatestring <- function(x, dbg = TRUE, n.head = 3)
{
  stopifnot(is.integer(x))
  
  dates <- as.Date(x, origin = "1899-12-30")
  
  kwb.utils::printIf(
    dbg, 
    data.frame(
      number = utils::head(x, n.head), 
      date = utils::head(dates, n.head)
    ),
    "I converted as follows"
  )
  
  as.character(dates)
}

# .localeToDatestring ----------------------------------------------------------

.localeToDatestring <- function(
  x, format, prefix = "", locale = c("de_DE", "German")
)
{
  stopifnot(is.character(x))
  
  # Temporarily set the locale to German and reset on exit
  locale <- ifelse(kwb.utils::.OStype() == "unix", locale[1], locale[2])
  
  locale.now <- Sys.setlocale("LC_TIME", locale)
  
  on.exit(Sys.setlocale("LC_TIME", locale.now))
  
  # Convert the date strings to YYYY-MM-DD
  as.character(as.Date(paste0(prefix, x), format))
}

# niceStationNames -------------------------------------------------------------

#' Nice Station Names
#' 
#' Substitutions: Berlin -> Bln, Umlaut o -> oe, spaces removed
#' 
#' @param stations vector of character containing station (= gauge) names
#'   to be cleaned
#'   
niceStationNames <- function(stations)
{
  kwb.utils::multiSubstitute(stations, list(
    "Berlin" = "Bln", 
    "\xF6" = "oe",
    "\\s+" = ""
  ))
}

# gaugeNamesShort --------------------------------------------------------------

#' Rain Gauge Long Name to Short Name
#' 
#' @param colnames Column names read from Excel sheet
#' @param underscore.rm if TRUE, underscores are removed
#' 
#' @export
#' 
gaugeNamesShort <- function(colnames, underscore.rm = FALSE)
{
  ## Remove text parts matching the following patterns
  for (pattern in c("Regenmesser", " - ", "\\d\\d[#.]\\d\\d", "\\s")) {
    
    colnames <- gsub(pattern, "", colnames)
  }
  
  colnames <- gsub("\xF6", "oe", colnames)
  
  if (isTRUE(underscore.rm)) {
    
    colnames <- gsub("_", "", colnames)
  }
  
  colnames
}

# toGaugeInfo ------------------------------------------------------------------

#' BWB Rain Data Column Name to Gauge Info Table
#' 
#' Convert a vector of BWB rain data column names "\code{id} \code{name} -
#' Regenmesser" to a data frame with columns \code{id} and \code{<name>}
#' 
#' @param x vector of column names of the form "01.02 <gauge> - Regenmesser"
#'
toGaugeInfo <- function(x)
{
  pattern <- "^(\\d\\d\\.\\d\\d)\\s+(.*\\S)\\s+- Regenmesser$"
  
  isMatch <- grepl(pattern, x)
  
  if (! all(isMatch)) {
    
    stop(call. = FALSE, sprintf(
      "The following strings do not match the pattern '%s': %s",
      pattern, kwb.utils::stringList(x[! isMatch])
    ))
  }
  
  match.names <- c("Code", "Name")
  
  matches <- kwb.utils::subExpressionMatches(pattern, x, match.names)
  
  gauges <- kwb.utils::rbindAll(
    lapply(matches, data.frame, stringsAsFactors = FALSE)
  )
  
  gauges <- cbind(Longname = x, gauges, stringsAsFactors = FALSE)
  
  kwb.utils::resetRowNames(gauges[order(gauges$Code), ])
}
