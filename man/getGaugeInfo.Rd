% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rain_meta.R
\name{getGaugeInfo}
\alias{getGaugeInfo}
\title{Get Rain Gauge Coordinates from Rain Meta Database}
\usage{
getGaugeInfo(mdb = mdb_rain_meta())
}
\arguments{
\item{mdb}{mdb containing table "tblPwInfo"}
}
\value{
data frame with columns \code{pw, xpos, ypos}
}
\description{
Get Rain Gauge Coordinates from Rain Meta Database
}
