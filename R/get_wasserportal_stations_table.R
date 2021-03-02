#' Wasserportal Berlin: get overview options for stations
#'
#' @return list with shortcuts to station overview tables 
#' https://wasserportal.berlin.de/messwerte.php?anzeige=tabelle&thema=<shortcut>
#' @export
#'
#' @examples
#' get_overview_options()
#' 
get_overview_options <- function ()  {

  list(surface_water = list(water_level = "ws",
                            flow = "df",
                            level = "wt", 
                            conductivity = "lf", 
                            ph = "ph", 
                            oxygen_concentration = "og", 
                            oxygen_saturation = "os"),
       groundwater = list(level = "gws",
                          quality = "gwq")
       )
  
}


#' Wasserportal Berlin: get overview options for stations
#'
#' @param type type of stations table to retrieve. Valid options defined in 
#' get_overview_options(), default: get_overview_options()$groundwater$level 
#' @param url_wasserportal base url to Wasserportal berlin (default: 
#' wasserportal_base_url())
#' @return data frame with metadata for 
#' @export
#' @importFrom kwb.utils substSpecialChars
#' @importFrom rvest html_node html_table
#' @importFrom stringr str_remove_all
#' @importFrom xml2 read_html
#' @examples
#' types <- kwb.read::get_overview_options()
#' str(types)
#' sw_l <- kwb.read::get_wasserportal_stations_table(type = types$surface_water$water_level)
#' str(sw_l)

get_wasserportal_stations_table <- function (
  type = get_overview_options()$groundwater$level,
  url_wasserportal = wasserportal_base_url()
) {
  
  

       

  if (! is.null(type)) {
    type <- match.arg(type, unlist(get_overview_options()))
  }


overview_url <- sprintf("%s/messwerte.php?anzeige=tabelle&thema=%s", 
                        url_wasserportal, 
                        type)

html_overview <- xml2::read_html(overview_url)  
  
overview_table <-  html_overview %>%
  rvest::html_node(xpath = '//*[@id="pegeltab"]') %>% 
  rvest::html_table()

names(overview_table) <- stringr::str_remove_all(names(overview_table), 
                                                 "-") %>% 
  kwb.utils::substSpecialChars()

overview_table

}

