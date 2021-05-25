\name{The Trt data.}
\docType{data}
\alias{trt1}
\alias{trt2}
\title{Sensitivity Analysis for Binary Missing Data}
\description{These dataframes store data from a repeated measures
  clinical trial.  The data are simulated.  Trt1 contains data from
  the control arm and Trt2 contains data from the active arm.}
\details{In each of trt1 and trt2 a row represtents the outcome measure
  for one subject at each of 3 timepoints. The data are coded as 0
  or 1 for the binary outcome and as 2 for missing data.  The datasets
  are delibertly small and are used in the help examples.
  \itemize{
  \item y1 measure at baseline  time-point 1
  \item y2 measure at follow-up time-point 2
  \item y3 measure at follow-up time-point 3
  \item y4 measure at follow-up time-point 4
  \item y6 measure at follow-up time-point 5
  \item y6 measure at follow-up time-point 6
}
}
\usage{
data(trt1)
data(trt2)
}
\format{
  trt1 and trt2 are 300 by 6 dataframes
}

