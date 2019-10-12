if (FALSE)
{
  # Select one of the following sets of stations
  stations <- kwb.read::get_wasserportal_stations(type = "quality")
  stations <- kwb.read::get_wasserportal_stations(type = "flow")
  stations <- kwb.read::get_wasserportal_stations(type = "level")
  
  kwb.read::read_wasserportal(stations$Weisser_See)
  
  all_dfs <- lapply(stations, function(station) {
    try(kwb.read::read_wasserportal(station))
  })

  failed <- sapply(all_dfs, inherits, "try-error")
  
  stopifnot(all(! failed))
  
  all_dfs[failed]
  
  dfs <- all_dfs[! failed]

  # Show data sections where the 15 minute timestep is broken
  lapply(dfs, function(df) {
    diffs <- diff(df$LocalDateTime)
    kwb.utils::printIf(TRUE, table(diffs))
    indices <- which(diffs != 15)
    df[sort(unique(c(indices - 1, indices, indices + 1))), ]
  })
  
  data <- dplyr::bind_rows(dfs, .id = "station")
  
  data[data == -777] <- NA
  
  View(data)

  # Select manueally one of the following aesthetics 
  aes <- ggplot2::aes_string(x = "LocalDateTime", y = "Wassertemperatur")
  aes <- ggplot2::aes_string(x = "LocalDateTime", y = "Sauerstoffgehalt")
  aes <- ggplot2::aes_string(x = "LocalDateTime", y = "Durchfluss")
  aes <- ggplot2::aes_string(x = "LocalDateTime", y = "Wasserstand")
  
  ggplot2::ggplot(data, aes) + 
    ggplot2::geom_line() +
    ggplot2::facet_wrap("station")
}
