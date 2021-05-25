\name{salbmCombine}
\alias{salbmCombine}
\title{Sensitivity Analysis for Binary Missing Data}
\description{Combines main and bootstrap results.} 

\usage{salbmCombine(x0, Samps=NULL, div=c(NA,NA))}

\arguments{
  \item{x0}{ main results from original data.}

  \item{Samps}{ list of sample results }

  \item{div}{ low and high alpha points }
}

\details{
  Combines data and produces bootstrap summaries including confidence intervals.

  Four types of confidence intervals are calculated after combining the results.
  Fixing the sensitivity parameter, let \eqn{E} be an estimate for a treatment group
  and for \eqn{b=1,2,\ldots,B} let \eqn{E_b} be the corresponding estimate for bootstrap \eqn{b}.
  Let \eqn{SE} be a standard error estimate for \eqn{E} and \eqn{SE_b} be a standard error estimate
  for \eqn{E_b}.  Let \eqn{q( \{x_i\}, v)} be the vth quantile for the collection \eqn{\{x_i\}}.

1. Let \eqn{q_l = q( \{E_b\}, 0.025 )} and \eqn{q_u = q( \{E_b\}, 0.975 )}. Then a 95\% confdence
   interval for \eqn{E} is given by \eqn{( q_l, q_u )}.  These are refered to as c("lb1","ub1")
   or type 1 confidence intervals.

2. Let \eqn{q_s = q( \{\mid E_b - E\mid\}, 0.95 )}.  Then a 95\% confdence interval for \eqn{E} is 
   given by \eqn{( E - qs, E + qs )}.  These are refered to as c("lb2","ub2")
   or type 2 confidence intervals.

3. Let \eqn{t_b = ( E_b - E ) / SE_b}, eqn{q_l = q( \{t_b\}, 0.025 )} and \eqn{q_u = q( \{t_b\}, 0.975 )}.
   Let \eqn{M} be the mean of \eqn{E_b}.  Then a 95\% confdence  interval for eqn{E} is given by 
        \eqn{( M - q_u SE, M - q_l SE )}
   These are refered to as c("lb3","ub3") or type 3 confidence intervals.

4. Let \eqn{t_b = ( E_b - E ) / SE_b}, and \eqn{q_s = q( \{\mid t_b \mid\}, 0.95 )}.
   Let \eqn{M} be the mean of \eqn{E_b}.  Then a 95\% confdence  interval for E is given by 
        \eqn{( M - q_s SE, M + q_s SE )}
   These are refered to as c("lb4","ub4") or type 4 confidence intervals.

   Computing SE

   Let D be a dataset with n rows and T a fixed timepoint.  Three standard 
   deviations are computed 

   \eqn{SD_0} the standard deviation of the data in D at timepoint T when 0 is 
   substituted for missing values in D, 

   \eqn{SD_1} the standard deviation of the data in D at timepoint T when 1 is 
   substituted for missing values in D, and 

   \eqn{SD_m} the standard deviation of the data in D at timepoint T when mean
   value at time T is substituted for missing values at time T.

   A low, mid and  high values of alpha are choosen and denoted by \eqn{a_l}, \eqn{a_0}, and \eqn{a_u} respectively.
   In salbm \eqn{a_0 = 0}.  Then a standard error is computed as:

  \deqn{ SE = \left\{\Large\begin{array}{ll}
        \frac{( \alpha - a_l ) SD_0 + ( a_0 - \alpha ) SD_m }{ ( a_0 - a_l ) \sqrt{n} }&\normalsize \mbox{\hspace{0.2in} when } \normalsize \alpha < 0 \\[0.15in]

        \frac{( a_u - \alpha ) SD_1 + ( \alpha - a_0 ) SD_m }{ ( a_u - a_0 ) \sqrt{n} }&\normalsize \mbox{\hspace{0.2in} when } \normalsize \alpha \ge 0
   \end{array}
   \right.
  }
}

\value{
 a list with combined results
}

\seealso{
  salbm
}  
\examples{
\donttest{
  data(trt1)
  data(trt2)
  data <- list( trt1 = trt1, trt2 = trt2 )

  ## main run
  x0 <- salbm( data = data , K = 6, ntree = 250, 
               seeds = c(22,18), seeds2 = c(-2,-3),
               alphas = -5:5, NBootstraps=0 )

  ## add Bootstraps
  samp1 <- addSamples(obj=x0, seeds=c(99,12), 
               seeds2 = c(-45,-80), bBS=1,
               NBootstraps=250)
  ## add more Bootstraps
  samp2 <- addSamples(obj=x0, seeds=c(9,2), 
               seeds2 = c(-54,-8), bBS=251,
               NBootstraps=250)

  ## Together
  R <- salbmCombine(x0=x0, Samps=list(samp1,samp2))
}
}

