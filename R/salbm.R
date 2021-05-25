salbm <- function( data, Narm = length(data), K, ntree, 
                   seeds = 1:length(data), seeds2 = -1 - 1:length(data), 
                   alphas, NBootstraps = 0, bBS = 1, 
                   returnJP = TRUE, returnSamples = FALSE)
{
  ns  <- length(seeds)
  ns2 <- length(seeds2)
  if ( ns  < Narm ) seeds  <- c( seeds,  seeds [ns ] + 1:(Narm-ns ))
  if ( ns2 < Narm ) seeds2 <- c( seeds2, seeds2[ns2] - 1:(Narm-ns2))
  eBS <- bBS + NBootstraps - 1

  for ( trt in 1:Narm ) {
    tdat <- data[[trt]]
    tdat[ is.na(tdat) ] <- 2
    data[[trt]] <- tdat
  }

  Ret  <- list( data = data, Narm = Narm, K = K, ntree = ntree,
                seeds = seeds, seeds2 = seeds2, alphas = alphas,
                bBS = bBS,  eBS = eBS, NBootstraps = NBootstraps)

# --------------------------------------------------------------------------------
  Ret[["mna" ]] <- min(alphas)
  Ret[["mxa" ]] <- max(alphas)
# --------------------------------------------------------------------------------
  for ( trt in 1:Narm ) {
    sd               <-  seeds2[trt]
    tdat             <-  data[[trt]]
    nr               <-  nrow(tdat)
    wts              <-  wtsDat( tdat, sub = 0, trt = trt)

    tdat[]           <-  lapply(tdat, function(x) factor(x, levels=c("0","1","2")))
    jp               <-  rfjp( data = tdat, ntree = ntree, seed = sd, nodesize = 1 )

    tiltRes          <-  lapply( alphas, function(x) tilt(x, jp) )
    trtR             <-  do.call(rbind,tiltRes)
    trtR             <-  as.data.frame(trtR)
    names(trtR)      <-  c("alpha", paste( "E", 1:K, sep=""), paste( "Esum", 1:K, sep=""))
    trtRL            <-  mkRLong( trtR, K, trt=trt )

    nms              <-  c( paste0("Main",trt,"R"), paste0("Main",trt,"RL"), paste0("Main",trt,"wts") )

    Ret[[nms[1]]]    <-  trtR
    Ret[[nms[2]]]    <-  trtRL
    Ret[[nms[3]]]    <-  wts

    if ( returnJP ) {
       Ret[[ paste0("JP",trt) ]] <- jp
    }

    if ( NBootstraps > 0 ) {
      set.seed( seeds[trt] )
      llout   <- lapply( bBS:eBS, oneSamp, jps=jp, nsamp = nr, K = K, 
                         sd = sd, ntree = ntree, alphas = alphas, trt = trt, 
                         returnSamples = returnSamples)

      SampR   <- lapply(llout,function(x) { return(x$SampR   ) } )
      SampRL  <- lapply(llout,function(x) { return(x$SampRL  ) } )
      Sampwts <- lapply(llout,function(x) { return(x$wtsSamp ) } )

      SampR      <- do.call(rbind,SampR)
      SampRL     <- do.call(rbind,SampRL)
      Sampwts    <- do.call(rbind,Sampwts)

      nms            <-  c( paste0("Samp",trt,"R"),   paste0("Samp",trt,"RL"), 
                            paste0("Samp",trt,"wts"), paste0("Samp",trt))
      Ret[[nms[1]]]  <-  SampR
      Ret[[nms[2]]]  <-  SampRL
      Ret[[nms[3]]]  <-  Sampwts

      if ( returnSamples ) {
         Samp   <- lapply(llout,function(x) { return(x$Samp  ) } )
         Samp   <- do.call(rbind,Samp)
         Ret[[nms[4]]] <- Samp
      }
    }  
  }
# --------------------------------------------------------------------------------
  class(Ret) <- c("salbm")
  return(Ret)
}
