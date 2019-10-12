if (FALSE)
{
  stations <- kwb.read::get_wasserportal_stations(type = "quality")
  
  df <- kwb.read::read_wasserportal(station = stations[1])
  df[is.na(df$Sauerstoffsaettigung), ]
  
  View(df)
  
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
  
  kwb.datetime::isValidTimestampSequence(times)
  
  #data$Datum <- as.POSIXct(quality_data$Datum, format = "%d.%m.%Y %H:%M")
  
  ggplot2::ggplot(
    quality_data, ggplot2::aes_string(
      x = "Datum", y = "Leitfaehigkeit", col = "station"
    )
  ) + ggplot2::geom_line()
  
  # Continue with one time series
  quality <- qualities[[1]]
  
  # Check number of values per day
  dates <- quality$Datum
  
  head(dates)
  tail(dates)
  
  n_per_day <- table(substr(dates, 1, 10))
  n_per_day[n_per_day != 96]
  
  dates[which(substr(dates, 1, 10) == "09.10.2018")]
  
  # Show days of time switch  
  switch_days <- kwb.datetime::reformatTimestamp(
    kwb.datetime::date_range_CEST(2019), 
    old.format = "%Y-%m-%d", new.format = "%d.%m.%Y"
  )
  
  pattern <- paste(switch_days, "0[1-4]", collapse = "|")
  
  quality[grepl(pattern, dates), ]
}
