\name{salbm}
\alias{salbm}
\title{Sensitivity Analysis for Binary Missing Data}

\description{For a list of dataframes, where each frame is
  of the form (Y_1,Y_2, ..., Y_K) and Y_t takes the values
  0, 1, or 2 (missing), salbm estimates E[ Y_t | alpha ]
  where alpha is one of a number of sensitivity paramaters.}

\usage{
salbm( data, Narm = length(data), K, ntree, 
       seeds = 1:length(data), seeds2 = -1 - 1:length(data), 
       alphas, NBootstraps = 0, bBS = 1,
       returnJP = TRUE, returnSamples = FALSE) 
}

\arguments{
  \item{data}{ a list of dataframes}

  \item{Narm}{ the number of dataframes to process}

  \item{K}{ the number of time-points}

  \item{ntree}{ the number of trees in the random forest passed to
    randomForestSRC} 
  
  \item{seeds}{ vector of positive numbers used as seeds in producing
   bootstrap samples.  There should be at least one seed for each treatment
   arm.}
  
  \item{seeds2}{ vector of negative numbers passed to randomForestSRC. There
  should be at least one seed for each treatment arm.}

  \item{alphas}{ vector of sensitivity parameters}

  \item{NBootstraps}{ number of bootstrap samples to be
    created and analyzed}

  \item{bBS}{ Start bootstrap number. Bootstrap ids are given as bBS:eBS
    where eBS = bBS + NBootstraps - 1.  Setting bBS to a value other than
    1 is useful when running salbm in parallel.}

  \item{returnJP}{ Logical indicating if the joint probability distribution
    for each treatment group should be returned.  This can be used by addSamples
    to create Bootstrap samples.}

  \item{returnSamples}{ Logical indicating if generated bootstrap samples should be
    returned}
}

\details{
 The list data contains Narm dataframes each of which has the form
 (Y_1,Y_2, ..., Y_K) where each Y_t takes the value 0 or 1 when Y_t
 is observed and has value 0 or 1 respectively and Y_t takes the 
 value NA (or 2) when Y_t is not observed.
   
 For each dataframe separately, randomForestSRC is used to create the
 joint probability distribution f(Y_1,Y_2, ..., Y_K) where Y_t can take
 three possible values, 0, 1, or missing (represented by the value 2).

 This joint distributins is "tilted" once for each sensitivity
 parameter alpha to produce the fully observed joint distribution,
 t(Y_1,Y_2, .... Y_K | alpha ).

 Statistics such as E[ Y_K | Y_K is fully observed, alpha ] can 
 then be computed.

 Precision estimates are calculated using the bootstrap with
 bootstraps generated from  f(Y_1,Y_2, ..., Y_K). 

 Even for quite modest values of K the number of possible combinations
 of \{Y_i\} can be large and the representation in memory of the joint
 distribution function, f, can be problematic.
 For larger sized values of K it might be worth considering
 placing a Markovian assumption on the distributions of f(Y_j) and
 this can be analysed using salbmM. 
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
  salbmM, addSamples.
}  
\examples{
  data(trt1)
  data(trt2)
  data <- list( trt1 = trt1[,1:3], trt2 = trt2[,1:3] )

  R <- salbm( data = data, K = 3, ntree = 500, 
              seeds = c(22,18), seeds2 = c(-2,-3),
              alphas = -1:1, NBootstraps=0 )
}

