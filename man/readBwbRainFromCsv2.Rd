% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bwb_rain_data.R
\name{readBwbRainFromCsv2}
\alias{readBwbRainFromCsv2}
\title{Read BWB rain data from CSV file}
\usage{
readBwbRainFromCsv2(
  file,
  sep = ";",
  country = "de",
  date.format = "\%d.\%m.\%Y \%H:\%M",
  dbg = TRUE
)
}
\arguments{
\item{file}{full path to CSV file}

\item{sep}{column separator}

\item{country}{"en" for English number format, "de" for German number format}

\item{date.format}{date format string, passed to
\code{\link[kwb.datetime]{reformatTimestamp}}}

\item{dbg}{if \code{TRUE} (default), debug messages are shown}
}
\description{
Read BWB rain data from CSV file
}
