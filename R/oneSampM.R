## one samp
oneSampM <- function( sub, jps, trt, m, nsamp, K, sd, ntree, EmpEst = EmpEst, NEst, alphas, returnSamples = FALSE ) { 
   Samp           <-   mkSamp( this.jp = jps , trt=trt, m=m, nsamp = nsamp, K = K )
   Samp           <-   dfac(Samp)

   wtsSamp        <-   wtsDat( Samp, sub = sub, trt = trt)

   Samp[]         <-   lapply( Samp, function(x) factor(x,levels=c("0","1","2")) )
   jpsamp         <-   mkjps( indata = Samp, m = m, bigK = K, seed = sd-sub, ntree = ntree )
   laout          <-   lapply( alphas, MarkovAl, jps = jpsamp, K = K, m = m, EmpEst = EmpEst, NEst=NEst )
   SampR          <-   do.call(rbind,laout)
   SampRL         <-   mkRLongM( SampR, K, trt=trt )
   SampR          <-   as.data.frame(SampR)
   SampRL         <-   as.data.frame(SampRL)  
   SampR [,"sub"] <-   sub
   SampRL[,"sub"] <-   sub
   Samp  [,"sub"] <-   sub
   if ( returnSamples == TRUE ) rlist <- list( SampR = SampR, SampRL = SampRL, wtsSamp = wtsSamp, Samp = Samp )
   else  rlist <- list( SampR = SampR, SampRL = SampRL, wtsSamp = wtsSamp )
   return( rlist )
} 
