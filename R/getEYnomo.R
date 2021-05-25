#    Expected difference in Y 100*(non-observed - observed)/observed.  
# or Expected difference in Y 100*(non-observed/observed).  
getEYnomo <- function( Y, BC, ratio=FALSE ) {
  n0 <- length(Y)

  nobs       <- sum( !is.na(Y) ) 
  nnonobs    <- sum(  is.na(Y) )
  EYobs      <- mean(Y[ !is.na(Y) ])

  EYnonobs   <- ( BC - EYobs * ( nobs / n0 )) / ( nnonobs / n0 )
  difference <- EYnonobs - EYobs
  if ( ratio == TRUE ) out  <- 100 * EYnonobs / EYobs
  else out <- 100 * (EYnonobs - EYobs)/EYobs
  return(out)
}
