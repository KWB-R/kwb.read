% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/senates_grab_sample_data_1.R
\name{hsGsData1ToList}
\alias{hsGsData1ToList}
\title{Transform Grab Sample Data (Type 1)}
\usage{
hsGsData1ToList(gsData1, namesAsFactor = FALSE, dbg = TRUE)
}
\arguments{
\item{gsData1}{grab sample data structure as returned by
\code{\link{hsGetGsData1}}}

\item{namesAsFactor}{passed to \code{\link[kwb.utils]{rbindAll}}}

\item{dbg}{if \code{TRUE} (default), debug messages are shown}
}
\description{
Transform grab sample data (type 1) as returned by \code{\link{hsGetGsData1}}
to "all-in-one-table"-format
}
\seealso{
\code{\link{hsGetGsData1}, \link{hsGsData2ToList}}
}
