if (FALSE)
{
  stations <- kwb.read::get_wasserportal_stations(type = "quality")
  
  df <- kwb.read::read_wasserportal(station = stations$MS_Schmoeckwitz)

  View(df)
  
  df[sort(unique(unlist(lapply(df, function(x) which(is.na(x)))))), ]
  
  all_dfs <- lapply(stations, function(station) {
    try(kwb.read::read_wasserportal(station))
  })

  failed <- sapply(all_dfs, inherits, "try-error")
  
  all_dfs[failed]
  
  dfs <- all_dfs[! failed]

  lapply(dfs, function(df) {
    diffs <- diff(df$LocalDateTime)
    kwb.utils::printIf(TRUE, table(diffs))
    indices <- which(diffs != 15)
    df[sort(unique(c(indices - 1, indices, indices + 1))), ]
  })
  
  data <- dplyr::bind_rows(dfs, .id = "station")
  
  data[data == -777] <- NA
  
  View(data)

  df <- dfs$Tiefwerder
  
  kwb.datetime::isValidTimestampSequence(df$LocalDateTime)
  
  ggplot2::ggplot(
    data, ggplot2::aes_string(x = "LocalDateTime", y = "Durchfluss")
  ) + ggplot2::geom_line() +
    ggplot2::facet_wrap("station")
  
}
