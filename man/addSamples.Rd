\name{addSamples}
\alias{addSamples}
\title{Sensitivity Analysis for Binary Missing Data}
\description{This is a generic method which adds bootstrap samples together with their associated results to an object of class salbm or salbmM } 
\usage{addSamples(obj, NBootstraps = 0, bBS = 1, nseeds = c(5,9), nseeds2 = c(-4,-5), 
                 returnJP = TRUE, returnSamples = FALSE, ...)
}
\arguments{
  \item{obj}{ an object of class salbm or salbmM}

  \item{NBootstraps}{ number of bootstraps requested }

  \item{bBS}{ beginning index of bootstraps }

  \item{nseeds}{ a numeric vector of length 2 to be used in makeing subsamples }

  \item{nseeds2}{ a numeric vector of length 2 passed to randomForestSRC }

  \item{returnJP}{ a logical, if true, the item, jps is copied to the output object }

  \item{returnSamples}{ a logical, if true, the bootstrap samples are added to the output object }

  \item{...}{ additional parameters passed to methods }
}

\details{
 If the input object, obj, is of type salbm, then obj will contain an items jps1 and jps2 which are
 the joint probability distributions of Y_1, ..., Y_K derived from the runs of randomForestSRC
 applied to treatment 1 and treatment 2 respecctively.
 If the input object, obj, is of type salbmM, then obj will contain an items jps1 and jps2 which are
 a series of probability distributions derived from randomForestSRC under the Markovian assumption
 paramaterized by the integer m.

 In either case, these jps1 and jps2 are used to create NBootstrap samples for treatment 1 and treatment 2
 respectively..  Each bootstrap sample is analysed  using either salbm or salbmM.  

 All results in the original object, obj, are copied to the output object except that any new bootstrap 
 results are appended to the original bootstrap results.

 The parameter NBootstraps determines the number of bootstraps to create.  The sBS parameter allows the 
 new bootstrap results to be be indexed by sBS:(sBS+NBootstraps-1).  This is useful when running addSamples
 in parallel.

 nseeds is of the form c(sd1,sd2).  set.seed(sd1) is run once before bootstraps from treatment group 1 are 
 created and analyzed and set.seed(sd2) is run once before bootstraps from treatment group 2 are creaded and
 analyzed.  

 nseeds2 is also of the form c(sd1,sd2). The values of sd1 and sd2 should be negative.  The index for a given
 bootstrap sample (a value between sBS and sBS+NBootstraps-1) is subtracted from sd1 and passed to 
 randomForestSRC when a bootstrap from treatment group 1 is analyzed and is subtraced from sd2 and passed to
 randomForestSRC when a bootstrap from treatment group 2 is analyzed.

 The returnJP controls whether jp1 and jp2 are copied to the output object.  Since these can be large, it is
 often advisable to set returnJP = FALSE. 
}

\value{
 salbm returns a list which contains the following:
 \item{Main1R}{ results for treatment group 1 in wide format }

 \item{Main1RL}{ results for treatment group 1 in long format }

 \item{Main1wts}{ means and standard deviations for trt1 }
 
 \item{jps1}{ joint distribution returned from randomForestRSC, trt 1}

 \item{Samp1R}{ results for bootstrap samples trt1 in wide format }

 \item{Samp1RL}{ results for bootstrap samples trt1 in long format }

 \item{Samp1wts}{ means and standard deviations of bootstrap samples trt1.}

  \item{Main2R}{ results for treatment group 2 in wide format }

 \item{Main2RL}{ results for treatment group 2 in long format }

 \item{Main2wts}{ means and standard deviations for trt2 }
 
 \item{jps2}{ joint distribution returned from randomForestRSC trt 2}

 \item{Samp2R}{ results for bootstrap samples trt2 in wide format }

 \item{Samp2RL}{ results for bootstrap samples trt2 in long format }

 \item{Samp2wts}{ means and standard deviations of bootstrap samples trt2.}

 \item{data}{ the salbm data object supplied in the call to salbm}

 \item{K}{ the value of K supplied in the call to salbm}

 \item{ntree}{ the value of ntree supplied in the call to salbm}
 
 \item{alphas}{ the value of alphas supplied in the call to salbm}

 \item{seeds}{ the value of seeds supplied in the call to salbm}

 \item{seeds2}{ the value of seeds2 supplied in the call to salbm}

 \item{bBS}{ the value of bBS supplied in the call to salbm}

 \item{eBS}{ the value of eBS supplied in the call to salbm}

 \item{NBootstraps}{ the value of NBootstraps  supplied in the call to salbm}
}

\seealso{
  The salbn_userDoc.pdf file in the Examples subdirectory.
}  
\examples{
\donttest{
  # Clinical trial data with two arms placebo/dose.
 
  data <- list( trt1 = trt1, trt2 = trt2 )
  # original run - no bootstraps
  R <- salbm( data = data , K = 6, ntree = 1000, 
              seeds = c(22,18), seeds2 = c(-2,-3),
              alphas = -5:5, NBootstraps=0 )

  # add 100 bootstraps
  Rupd <- addSamples(R, NBootstraps=100, sBS=1,
            nseeds = c(81,80), nseeds2 = c(-6,-1),
            returnJP=FALSE)


  # Markov assumption m = 2 again no bootstraps
  RM <- salbmM( data = data , K = 6, m = 2, 
                ntree = 1000, seeds2 = c(-2,-3),
                alphas = -5:5, NBootstraps=0 )

  # add 100 bootstraps
  RMupd <- addSamples(RM, NBootstraps=100, sBS=1,
           nseeds = c(81,80), nseeds2 = c(-6,-1),
           returnJP=FALSE)

  # Markov assumption m = 3, empirical estimates 
  RME <- salbmMEst( data = data , K = 11, m = 3, 
                MEst = TRUE, NMest = 25000,
                ntree = 1000, seeds2 = c(-2,-3),
                alphas = -5:5, NBootstraps=100 )

  # and add 100 bootstraps
  RMEupd <- addSamples(RME, NBootstraps=100, sBS=101,
           nseeds = c(81,80), nseeds2 = c(-6,-1),
           returnJP=FALSE)
}
}

