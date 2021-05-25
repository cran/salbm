\name{salbmM}
\alias{salbmM}
\title{Sensitivity Analysis for Binary Missing Data}

\description{For a list of dataframes, where each frame is
  of the form (Y_1,Y_2, ..., Y_K) and Y_t takes the values
  0, 1, or 2 (missing), salbmM estimates E[ Y_t | alpha ]
  where alpha is one of a number of sensitivity paramaters
  under a Markovian assumption of order m.}

\usage{
salbmM( data, Narm = length(data), m, K, ntree, 
        EmpEst=FALSE, NEst=0,
        seeds = 1:length(data), seeds2 = -1 - 1:length(data), 
        alphas, NBootstraps = 0, bBS = 1, 
        returnJP = TRUE, returnSamples = FALSE )
}

\arguments{
  \item{data}{ a list of dataframes}

  \item{Narm}{ the number of dataframes to process}

  \item{m}{ order of the Markov assumption, note 2m+2 < K}
  
  \item{K}{ The number of time-points}

  \item{ntree}{ The number of trees in the random forest passed to
    randomForestSRC} 

  \item{EmpEst}{ logical, indicating if empirical estimation should be
  used when calculating the mean value of Yt.}
  
  \item{NEst}{ The number of values of Yt to use in calculating the mean
  of Yt.}
  
  \item{seeds}{ vector of positive numbers used as seeds in producing
   bootstrap samples.  There should be at least one seed for each treatment
   arm.}
  
  \item{seeds2}{ vector of negative numbers passed to randomForestSRC. There
  should be at least one seed for each treatment arm.}

  \item{alphas}{ vector of sensitivity parameters}

  \item{NBootstraps}{ number of bootstrap samples to be
    created and analyzed}

  \item{bBS}{ Start Bootstrap number. Bootstrap IDs are given as bBS:eBS
    where eBS = bBs + NBootstraps - 1. Setting bBS and eBS is useful 
    when running salbmM in parallel.}  

  \item{returnJP}{ Logical indicating if the list of joint probability
    distributions returned by random forest for each treatment group 
    should be returned. This is used by addSamples to create Bootstrap
    samples.}

  \item{returnSamples}{ Logical indicating if generated bootstrap samples should be
    returned}
}

\details{
 For each dataframe separately, randomForestSRC is used to create a set 
 of joint distributions f(Yn-m, Yn-m+1, ..., Yn-1, Yn, Yn+1, ... Yn+m+1) 
 where Yi can take three possible values, 0, 1, or missing (represented
 by the value 2).  The Markovian assumption of order m can be summarized
 as f( Y_n | Y_i, i = 1, 2, ..., n-1, n+1, ..., K) =
 f( Y_n | Y_i, i = max(1,n-m), ..., n-1, n+1, ..., min(n+m+1,K)) for
 n > 1.

 RandomForestSRC is used to estimate the joint distributions, 
 f_i( Y_n | Y_{n-m}, ..., Y_{n-1}, Y_{n+1}, ..., Y_{n+m+1}).
 For each sensitivity parameter, alpha, these distributions are
 used to compute the E[ Y_K | alpha ]
 Bootstraping is carried out using the  $f_i$.

 Because of the Markov assumption the full distribution f can be
 replaced by a set of distributions of order no more than 2m+2.  This
 allows estimation in situations where K is large and estimation of 
 the full joint distribution is unfeasable.
}

\value{
 salbmM returns a list which contains the following:
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

 \item{data}{ the salbm data object supplied in the call to salbmM}

 \item{m}{ the Markov paramater supplied in the call to salbmM}

 \item{K}{ the value of K supplied in the call to salbmM}

 \item{ntree}{ the value of ntree supplied in the call to salbmM}

 \item{NEst}{ the value of NEst supplied in the call to salbmM}
 
 \item{alphas}{ the value of alphas supplied in the call to salbmM}

 \item{seeds}{ the value of seeds supplied in the call to salbmM}

 \item{seeds2}{ the value of seeds2 supplied in the call to salbmM}

 \item{bBS}{ the value of bBS supplied in the call to salbmM}

 \item{eBS}{ the value of eBS supplied in the call to salbmM}

 \item{NBootstraps}{ the value of NBootstraps  supplied in the call to salbmM}
}

\examples{
\donttest{
  # Clinical trial data with two arms.
  data(trt1)
  data(trt2)
  data <- list( trt1 = trt1, trt2 = trt2 )

  R     <-  salbmM( data = data , m = 2, K = 6, ntree = 1000,
              seeds = c(22,18), seeds2 = c(-2,-3),
              alphas = -8:8, NBootstraps=0 )
}
}

