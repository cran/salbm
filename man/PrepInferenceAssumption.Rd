\name{PrepInferenceAssumption}
\alias{PrepInferenceAssumption}

\title{Sensitivity Analysis for Binary Missing Data}

\description{Given an object returned from salbmCombine this function prepares results 
  for presentation in a table.}

\usage{PrepInferenceAssumption(M, CItp=2, qt = c(0.025, 0.975), SampCIType=1, carm=2)}

\arguments{
  \item{M}{ is an object returned by the salbm function salbmCombine. It should contain
  confidence bands ECI and SCI and means and standard deviations from bootstraps 
  (Main1wts,  Main2wts, Samp1wts and Samp2wts).}

  \item{CItp}{ a number ranging from 1 to 4, indicating which confidence interval to use.}

  \item{qt}{ two values passed to quantile in producing confidence intervals.}

  \item{SampCIType}{ type of CI for samples}

  \item{carm}{ comparison arm}
}

\details{
 PrepInferenceAssumption is used in conjunction with influenceAssumption to produce 
 a summary of an salmb results object,

 It produces summeries for each arm of the study and the difference in estimates
 between arms under various assumptions:

 \bold{MCAR}{ when the mean value is substituted for missing at each timepoint}

 \bold{missing=0}{ when 0 is substituted for missing at each timepoint}

 \bold{missing=1}{ when 1 is substituted for missing at each timepoint}

 \bold{benchmark}{ results returned from salbm or salbmM} 
}

\value{
  a matrix which inferenceAssumption can display.
}

\seealso{
  salbm, salbmM, influenceAssumption
}  
\examples{
\donttest{
  # M is an object returned from salbm's salbmCombine function

  prep <- PrepInferenceAssumption( M )
  inferenceAssumption(prep)
}
}

