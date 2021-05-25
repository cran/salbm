# tilt the values in results by alpha 
tilt <- function( alpha, results, small = 1E-5 ) {

  Res      <- results
  len      <- length(results)
  nn       <- round(log(len,base=3))
  Ealpha   <- exp(alpha)

  for ( j in 1:nn ) {
    len  <- length(Res)
    s0   <- seq( 1, len, by = 3 )
    s1   <- s0 + 1
    s2   <- s0 + 2
    A    <- Res[ s0 ]  
    B    <- Res[ s1 ]  
    C    <- Res[ s2 ]
    T    <- A + Ealpha * B

    X0   <- (( A == 0 ) & ( B == 0 )) & ( C > 0 )
    X1   <- (( A  > 0 ) & ( B  > 0 )) & ( C > 0 )
    X2   <- (( A  > 0 ) & ( B == 0 )) & ( C > 0 )
    X3   <- (( A == 0 ) & ( B  > 0 )) & ( C > 0 )

    rt    <-  sum(B) / ( sum(A) + sum(B) )
    maxf  <-  sum(B) + sum(C)  
    minf  <-  sum(B)           

    if      ( abs(rt - minf) < small * sum(C)) rt1 <- minf+small
    else if ( abs(maxf - rt) < small * sum(C)) rt1 <- maxf-small
    else {
      emo  <-  ( maxf - rt ) / ( rt - minf )
      rt1  <-  minf + (maxf - minf) / ( 1 + emo*exp( -alpha ))
      if ( is.nan(rt1) ) rt1 <- (minf + maxf)/2
      if ( rt1 < (minf+small) ) rt1 <- minf+small
      if ( rt1 > (maxf-small) ) rt1 <- maxf-small  
    }

    TA  <-  sum(A)
    TB  <-  sum(B)
    TC  <-  sum(C)

    if ( any(X1) ) { 
      A[X1] <- A[X1] + A[X1] *          C[X1] / T[X1]
      B[X1] <- B[X1] + B[X1] * Ealpha * C[X1] / T[X1]
      CA <- sum(A) - TA
      CB <- sum(B) - TB
      pA <- CA / ( CA + CB )
      pB <- CB / ( CA + CB )
    } else {
      pA <- 1 - rt1
      pB <- rt1
    }
 
    if ( any(X0) ) { 
      A[X0] <- A[X0] + pA * C[X0]
      B[X0] <- B[X0] + pB * C[X0]
    }
    if ( any(X2) ) { 
      TmA   <- A[X2]
      TmB   <- A[X2] * ( TB / TA )
      TmB   <- A[X2] * ( rt1 - minf ) / ( maxf - rt1 )
      A[X2] <- A[X2] + TmA *          C[X2] / (TmA + Ealpha * TmB)
      B[X2] <- B[X2] + TmB * Ealpha * C[X2] / (TmA + Ealpha * TmB)
    }
    if ( any(X3) ) { 
      TmA   <- B[X3] * ( TA / TB )
      TmA   <- B[X3] * ( maxf - rt1 ) / ( rt1 - minf )
      TmB   <- B[X3]
      A[X3] <- A[X3] + TmA *          C[X3] / (TmA + Ealpha * TmB)
      B[X3] <- B[X3] + TmB * Ealpha * C[X3] / (TmA + Ealpha * TmB)
    }

    Res <- c(A,B)
  }
  w          <-  Res
  len        <-  length(Res)
  sm         <-  rep(0,len)
  Results    <-  rep(NA,2*nn+1)
  Results[1] <-  alpha
  for ( j in 1:nn ) {
    yj                 <-  rep(c(rep( 0, 2^(j-1) ), rep( 1, 2^(j-1))), len / (2^j)) 
    Results[j+1]       <-  weighted.mean( yj, w = w )
    sm                 <-  sm + yj
    Results[nn+1+j]    <-  weighted.mean( sm, w = w )
  }
  return(Results)
}
