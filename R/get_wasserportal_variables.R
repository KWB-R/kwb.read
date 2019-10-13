# get_wasserportal_variables ---------------------------------------------------

#' Get Names and IDs of the Variables of wasserportal.berlin.de
#' 
#' @param station station id. If given, only variables that are available for 
#'   the given station are returned.
#' @export
get_wasserportal_variables <- function(station = NULL)
{
  variables <- list(
    quality = c(
      Wassertemperatur = "t",
      Leitfaehigkeit = "l",
      pH_Wert = "p",
      Sauerstoffgehalt = "o",
      Sauerstoffsaettigung = "s"
    ),
    level = c(Wasserstand = "w"),
    flow = c(Durchfluss = "d")
  )
  
  types <- names(variables)
  
  if (! is.null(station)) {
    
    types <- types[sapply(types, function(type) {
      
      station %in% get_wasserportal_stations(type)
    })]
  }
  
  unlist(lapply(types, function(element) variables[[element]]))
}
