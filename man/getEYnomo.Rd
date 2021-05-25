\name{getEYnomo}
\alias{getEYnomo}
\title{Sensitivity Analysis for Binary Missing Data}
\description{returns the difference in the expected value of the outcome, Y, when Y is not observed and the expected value Y when Y is observed as a percentage of the expected value of Y when observed.}

\usage{getEYnomo( Y, BC, ratio=FALSE)}
\arguments{
  \item{Y}{ a vector of observed data including NAs.}

  \item{BC}{ a vector of salbm estimates}

  \item{ratio}{ if true return the ratio 100 * E[Y|non-obs] / E[Y|obs];
        otherwise return 100 * ( E[Y|non-obs] - E[Y|obs] )/ E[y|obs]}
}

\details{
  Typically, BC is a vector of salbm at different sensitivity
  parameters alpha.  The function computes: 
    100 (E[ Y | Y not observed ] - E[ Y | Y is observed ]) /
         E[ Y | Y is observed ]
  when ratio is FALSE and computes
    100 E[ Y | Y not observed ] / E[ Y | Y is observed ]
  when ratio is TRUE
}

\value{
  A vector containing
    100 (E[ Y | Y not observed ] - E[ Y | Y is observed ]) /
         E[ Y | Y is observed ]
  or
    100  E[ Y | Y not observed ] / E[ Y | Y is observed ]
}

\examples{
\donttest{
  # trt1 is one of two arms of a clinical trial with 
  # trt1[,K] being the measurments at the last timepoint
  # BC has two columns the first containing sensitivity
  # paramenters and the second their corrisponding salbm
  # estimates

  pdiff  <- getEYnomo( trt1[,K], BC[,2] )
  plot( x = BC[,1], y = pdiff )
}
}
