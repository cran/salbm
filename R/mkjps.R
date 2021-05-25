mkjps <- function( indata, m, bigK, seed = -1, ntree = 500 ) {
  jpout <- mkjp( indata = indata, m = m, bigK = bigK, seed = seed, ntree = ntree )

  jps  <- list()
  for ( t in 1:bigK ) {
    st         <- max( t - m,     1    )
    sp         <- min( t + m + 1, bigK )
    nm         <- paste("y",st:sp,sep="")
    pat        <- mkPatr3(length(nm))
    tv         <- jpout[[t + m + 1]]
    tv         <- cbind(pat,tv)
    tv         <- as.data.frame(tv)
    names(tv)  <- c( nm, "jp" )
    tv[,nm]    <- lapply(tv[,nm],function(x) factor(x,levels=c("0","1","2")) )
    jps[[t]]   <- tv
  }
  return(jps)
} 
