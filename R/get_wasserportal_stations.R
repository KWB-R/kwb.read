# get_wasserportal_stations ----------------------------------------------------

#' Get Names and IDs of the Stations of wasserportal.berlin.de
#' 
#' @param type one of "quality", "level", "flow"
#' @export
get_wasserportal_stations <- function(type = "quality")
{
  if (! is.null(type)) {
    type <- match.arg(type, c("quality", "level", "flow"))  
  }
  
  file <- "stations_wasserportal.csv"
  
  stations <- readPackageFile(file, fileEncoding = "UTF-8")
  
  get <- kwb.utils::selectColumns
  
  stations$id <- as.character(get(stations, "id"))
  stations$name <- kwb.utils::substSpecialChars(get(stations, "name"))
  
  is_available <- if (is.null(type)) {
    seq_len(nrow(stations))
  } else {
    nzchar(get(stations, type))
  }
  
  kwb.utils::toLookupList(data = get(stations, c("name", "id"))[is_available, ])
}
