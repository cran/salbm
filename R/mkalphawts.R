mkalphawts <- function( alphas, div )
{
   wts     <-  matrix(0,nrow=length(alphas), ncol=4) 
   wts[,1] <-  alphas

##   mina    <-  min(alphas)
##   maxa    <-  max(alphas)

   for ( i in 1:length(alphas) ) {
      alpha <- alphas[i]
      if ( alpha < 0 ) {
        wts[i,2] <- - alpha / abs(div[1])
        wts[i,3] <- 1 + alpha / abs(div[1])
        wts[i,4] <- 0
      } else if ( alpha == 0 ) {
        wts[i,2] <- 0
        wts[i,3] <- 1
        wts[i,4] <- 0
      } else {
        wts[i,2] <- 0
        wts[i,3] <- 1 - alpha / abs(div[2])
        wts[i,4] <- alpha / abs(div[2])
      }
   }
   return(wts)
}
