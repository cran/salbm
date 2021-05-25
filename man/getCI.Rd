\name{getCI}
\alias{getCI}
\title{Sensitivity Analysis for Binary Missing Data}
\description{Given an object returned by salbmCombine the getCI
  function extracts estimates and confidence intervals.}

\usage{getCI( Res, ci=c("lb4","ub4") )}

\arguments{
  \item{Res}{ an object returned by salbmM, salbm with confidence intervals}

  \item{ci}{ indicates which confidence interval to return. See the
             help for salbmCombine for the list of confidence interval
             types.}
}

\details{
 Looks for ECI and SCI in the input object.   
 ECI contains confidence bands for estimates of 
    E[ Yt | alpha ] and their differences.  
 SCI contains confidence bands for estimates of the cumulative sum
    E[ Y1+Y2+...+YK | alpha ]
 and their differences.}

\value{
 returns a list with
  1. K  the value of the last timepoing
  2. alphas  the vector of sensitivity parameters
  3. ER1   an n by 4 matrix where n is the length of the alphas
           vector.  ER1 contains expected values of Y at time K
           for the first treatment group trt1.  The columns are
                 alpha, E[Y_K| alpha],  lb, ub
           where lb and ub are the lower bound and upper bound 
           of the confidence interval chosen.
  4. ER2   as ER1 but for the second treatment arm trt2.
  5. ERD   the difference in estimates trt2 - trt1.
  6. SR1   cumulative sum estimates for trt 1
  7. SR2   cumulative sum estimates for trt 2
  8. SRD   differences in cumulative sum estimates trt 2 - trt 1. 
}

\examples{
\donttest{
  # select confidence interval type 
  Res  <-  getCI( R, ci=c("lb2","ub2"))
  ER1  <-  Res$ER1
  plot( x = ER1[,1], y = ER1[, 2] )
}
}

