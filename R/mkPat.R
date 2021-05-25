# ----------------------------------------------------------
# Given n an integer, create a matrix with n columns
# containing all combinations of 0 and 1 in the first column
# and all combinations of 0,1 and 2 in the other columns.
# ----------------------------------------------------------
mkPat <- function( n ) {
    
  nc      <- n
  nr      <- 2 * 3 ^ (n-1)
  vals    <- c(0,1,2)

  mat     <- matrix(NA,ncol=nc,nrow=nr)
  mat[,1] <- sapply(c(0,1),rep,nr/2)

  if ( n > 1 ) {  
    for ( k in 1:(n-1) ) {
      mat[,k+1] <- rep(sapply(vals,rep,3^(n-1-k)), 2*3^(k-1))
    }
  }
  return(mat)
}

# ----------------------------------------------------------
# Given n an integer, create a matrix with n columns
# containing all combinations of 0, 1 and 2.
# ----------------------------------------------------------
mkPat3 <- function( n ) {
    
  nc      <- n
  nr      <- 3 ^ n
  vals    <- c(0,1,2)

  mat     <- matrix(NA,ncol=nc,nrow=nr)
  mat[,1] <- sapply(vals,rep,nr/3)

  if ( n > 1 ) {  
    for ( k in 1:(n-1) ) {
      mat[,k+1] <- sapply(vals,rep,3^(n-k-1))
    }
  }
  return(mat)
}


# ----------------------------------------------------------
# Given n an integer, create a matrix with n columns
# containing all combinations of 0, 1.
# ----------------------------------------------------------
mkPat2 <- function( n ) {
    
  nc      <- n
  nr      <- 2 ^ n
  vals    <- c(0,1)

  mat     <- matrix(NA,ncol=nc,nrow=nr)
  mat[,1] <- sapply(vals,rep,nr/2)

  if ( n > 1 ) {  
    for ( k in 1:(n-1) ) {
      mat[,k+1] <- sapply(vals,rep,2^(n-k-1))
    }
  }
  return(mat)
}
