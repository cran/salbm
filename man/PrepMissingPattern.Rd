\name{PrepMissingPattern}
\alias{PrepMissingPattern}
\title{Sensitivity Analysis for Binary Missing Data}
\description{Given a dataframe Y in which columns represent timepoints
  and rows represent subjects creates missing data summaries for display
  using missingPattern.}
\usage{PrepMissingPattern(Y)}
\arguments{
  \item{Y}{ Is an N0 by NT dataframe with each column representing timepoints
  and rows representing subjects.  Individual values Y_ij = 0, when the 
  event of interest has not occured for subject i at time j, Y_ij = 1, when
  the event of interest has occured for subject i at time j, Y_ij = 2 or NA
  when the measurement of the event of interest for subject i at time j is
  missing.}
}

\details{
 PrepMissingPattern is used in conjunction with missingPattern to produce a simple summary
 of missing data in a salmb dataframe.
}

\value{
  Returns a list with items
  \item{Mat}{ a matrix of frequencies and percentages}

  \item{tots}{ a matrix of subtotals }

  \item{K}{ the number of timepoints (columns) in the original data}
}

\examples{
  data(trt1)
  prep1 <- PrepMissingPattern(trt1)
}

