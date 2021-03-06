% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bwb_rain_corr_wide.R
\name{splitIntoBlocks}
\alias{splitIntoBlocks}
\title{Split Character Vector Into Blocks of Lines}
\usage{
splitIntoBlocks(x, pattern, offset.start = 0, offset.stop = 0)
}
\arguments{
\item{x}{vector of lines of character}

\item{pattern}{pattern maching the start lines of the blocks}

\item{offset.start}{offset added to the indices matching the block starts to
identify the first lines of the blocks to be cut}

\item{offset.stop}{offset defining the end of the blocks. A value of zero 
(default) means that a block ends one line before the start of the next 
block (a line matching \code{pattern})}
}
\description{
Split Character Vector Into Blocks of Lines
}
