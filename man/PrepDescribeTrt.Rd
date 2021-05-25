\name{PrepDescribeTrt}
\alias{PrepDescribeTrt}
\title{Sensitivity Analysis for Binary Missing Data}
\description{Given a dataframe Y in which columns represent timepoints
  and rows represent subjects creates summaries for display.}
\usage{PrepDescribeTrt(Y)}
\arguments{
  \item{Y}{ Is an N0 by NT dataframe with each column representing timepoints
  and rows representing subjects.  Individual values Y_ij = 0, when the 
  event of interest has not occured for subject i at time j, Y_ij = 1, when
  the event of interest has occured for subject i at time j, Y_ij = 2 or NA
  when the measurement of the event of interest for subject i at time j is
  missing.}
}

\details{
 PrepDescriveTrt is used in conjunction with describeTrt to produce a simple summary
 of a salmb dataframe.
}

\value{
 PrepDescribeTrt returns a table with 10 columns: 
    1. t
    2. Number On Study
    3. Number Observed
    4. last seen
    5. Proportion last seen (of on-study) 
    6. Proportion last seen (of observed)
    7. inter miss
    8. proportion inter missing (of onstudy)
    9. mean observed
   10. std observed
}

\seealso{
  salbm, salbmM
}  
\examples{
  data(trt1)

  # switch the 2s to NAs for tables.
  trt1[ trt1 == 2 ] <- NA
  prep1 <- PrepDescribeTrt(trt1)
}

