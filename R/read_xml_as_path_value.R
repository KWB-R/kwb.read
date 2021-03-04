# read_xml_as_path_value -------------------------------------------------------

#' Read XML File as Pairs of Paths and Values
#' 
#' @param xml path, url or literal xml or anything else that is accepted by
#'   \code{\link[xml2]{read_xml}} as argument \code{x}
#' @param dbg if \code{TRUE}, debug messages are shown
#' @param max_length maximum number of characters reserved for printing the
#'   path to the folder of the \code{xml} file in the debug message
#' 
#' @return data frame with the full paths to the XML elements in the first
#'   and the text values of the XML elements in the second column.
#' 
#' @export
#' @importFrom xml2 read_xml xml_children xml_ns_strip xml_path xml_text
#' @examples
#' url <- "https://www.w3schools.com/xml/note.xml"
#' 
#' # Original XML content
#' kwb.utils::catLines(readLines(url, warn = FALSE))
#' 
#' # Interpretation as Paths and Values
#' kwb.read:::read_xml_as_path_value(url)
#' 
read_xml_as_path_value <- function(xml, dbg = TRUE, max_length = 100)
{
  kwb.utils::catIf(dbg, sprintf(
    "Reading '%s' in folder\n  '%s' ... ", 
    basename(xml), 
    kwb.utils::shorten(dirname(xml), max_length, "[...]")
  ))
  
  x <- xml2::read_xml(xml)
  
  kwb.utils::catIf(dbg, "ok.\n")
  
  xml2::xml_ns_strip(x)
  
  get_paths_and_values(x)
}

# get_paths_and_values ---------------------------------------------------------
get_paths_and_values <- function(x, as_matrix = FALSE, depth = 0)
{
  children <- xml2::xml_children(x)
  
  result <- if (length(children)) {
    
    do.call(rbind, lapply(children, get_paths_and_values, depth = depth + 1))
    
  } else {
    
    c(path = xml2::xml_path(x), value = xml2::xml_text(x))
  }
  
  if (depth == 0 && ! as_matrix) {
    
    as.data.frame(result, stringsAsFactors = FALSE)
    
  } else {
    
    result
  }
}
