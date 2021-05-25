\name{getXCI}
\alias{getXCI}
\title{Sensitivity Analysis for Binary Missing Data}
\description{Given an object returned by salbmCombine the getXCI
  function extracts difference estimates and confidence intervals
  by pairs of sensitivity paramaters.}

\usage{getXCI( Res, ci=c("lb4","ub4") )}

\arguments{
  \item{Res}{ an object returned by salbmCombine with confidence intervals}

  \item{ci}{ indicates which confidence interval to return. See the
             help for salbmCombine for the list of confidence interval
             types.}
}

\details{
 Looks for ECI and SCI in the input object. For each pair of sensitivity parameters
 ECI contains confidence bands for estimates of 
    E[ Yt | alpha ] and their differences.  
 SCI contains confidence bands for estimates of the cumulative sum
    E[ Y1+Y2+...+YK | alpha ]
 and their differences.}

\value{
 returns a list with
  1. K  the value of the last time-point
  2. alphas  the vector of sensitivity parameters
  3. ER1   an n^2 by 5 matrix where n is the length of the alphas
           vector.  ER1 contains expected values of Y at time K
           for the first treatment group trt1.  The columns are
                 alpha1, alpha2, E[Y_K| alpha],  lb, ub
           where lb and ub are the lower bound and upper bound 
           of the confidence interval chosen, alpha1 is the
           sensitivity parameter associated with treatment group 1
           and alpha2 the sensitivity parameter associated with
           treatment group 2.
  4. ER2   as ER1 but for the second treatment arm trt2.
  5. ERD   the difference in estimates trt2 - trt1.
  6. SR1   cumulative sum estimates for trt 1
  7. SR2   cumulative sum estimates for trt 2
  8. SRD   differences in cumulative sum estimates trt 2 - trt 1. 
}

\examples{
\donttest{
  fplots <- getXCI( Res=Results, lb=c("lb2","ub2") )
  alphas <- fplots$alphas
  ERD    <- fplots$ERD

  ## significant points
  Epts   <-  ERD[ sign(ERD[,4]) == sign(ERD[,5]), ]

  filled.contour( x = alphas, y = alphas,
     z              = matrix(ERD[,3],length(alphas),length(alphas), byrow=TRUE),
     xlab           = expression(paste(alpha, " (Treatment 1)")),
     ylab           = expression(paste(alpha, " (Treatment 2)")),
     nlevels        = 8,
     color.palette  = colorRampPalette(c( "#993404","#D95F0E","#FE9929","#FFD9BE","#FFFFD4"),
                                       space="rgb"),
     plot.axis      = ( points( Epts[,c(1,2)], pch=15, cex=0.8, col = "#777799FF" ) )
  ) 
}
}

