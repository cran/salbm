mkRLong <- function(trtR,K,trt) {
  trtRL <- matrix(NA, nrow=nrow(trtR)*2*K, ncol = 5 )
  cnt   <- 0 
  for ( i in 1:nrow(trtR) ) {
    alpha  <- trtR[i,1]
    for ( j in 1:K ) {
       cnt         <- cnt + 1
       Est         <- trtR[i,j+1]
       type        <- 1
       trtRL[cnt,] <- c( trt, alpha, type, Est, j )

       cnt         <- cnt + 1
       Est         <- trtR[i,K+j+1]
       type        <- 2
       trtRL[cnt,] <- c( trt, alpha, type, Est, j )
    }
  }
  colnames(trtRL) <- c("trt","alpha","type","Est","k")
  return(trtRL)
}
