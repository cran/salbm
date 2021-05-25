## one samp
oneSamp <- function( sub, jps, nsamp, K, sd, ntree, alphas, trt, returnSamples=FALSE ) { 
  pat                 <-      mkPatr3(K)
  jps                 <-      cbind( pat, jps)
  indx                <-      sample( 1:nrow(jps), nsamp, prob = jps[,ncol(jps)], replace=TRUE)
  Samp                <-      jps[ indx, 1:K ]
  colnames(Samp)      <-      paste("y",1:K,sep="")
  Samp                <-      as.data.frame(Samp)
  Samp                <-      dfac(Samp)
  Samp                <-      as.data.frame(Samp)
  wtsSamp             <-      wtsDat( Samp, sub = sub, trt = trt)

  Samp[]              <-      lapply( Samp, function(x) factor(x,levels=c("0","1","2")) )
  jp                  <-      rfjp( data = Samp, ntree = ntree, seed = sd-sub, nodesize = 1 )
  tiltRes             <-      lapply( alphas, function(x) tilt(x, jp))
  SampR               <-      do.call(rbind,tiltRes)
  SampR               <-      as.data.frame(SampR)
  names(SampR)        <-      c("alpha", paste( "E", 1:K, sep=""), paste( "Esum", 1:K, sep=""))
  
  SampRL              <-      mkRLong( SampR, K, trt=trt )
  SampR               <-      as.data.frame(SampR)
  SampRL              <-      as.data.frame(SampRL)  
  SampR [,"sub"]      <-      sub
  SampRL[,"sub"]      <-      sub
  Samp  [,"sub"]      <-      sub

  rlist <- list( SampR = SampR, SampRL = SampRL, wtsSamp = wtsSamp)
  if ( returnSamples ) rlist[["Samp"]] <- Samp
  return( rlist )
} 
