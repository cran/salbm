salbmM <- function( data, Narm = length(data), m, K, ntree, 
                    EmpEst=FALSE, NEst=0,
                    seeds = 1:length(data), seeds2 = -1 - 1:length(data), 
                    alphas, NBootstraps = 0, bBS = 1, 
                    returnJP = TRUE, returnSamples = FALSE )
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

  Ret  <- list( data = data, Narm = Narm, m = m, K = K, ntree = ntree,
                EmpEst = EmpEst, NEst = NEst,
                alphas = alphas, seeds = seeds, seeds2 = seeds2, 
                bBS = bBS, eBS = eBS, NBootstraps = NBootstraps )

# --------------------------------------------------------------------------------
  Ret[["mna" ]] <- min(alphas)
  Ret[["mxa" ]] <- max(alphas)
# --------------------------------------------------------------------------------
  for ( trt in 1:Narm ) {
    sd              <-   seeds2[trt]
    tdat            <-   data[[trt]]
    nr              <-   nrow(tdat)
    wts             <-   wtsDat( tdat, sub = 0, trt = trt)
    tdat[]          <-   lapply( tdat, function(x) factor(x,levels=c("0","1","2")) )
    jps             <-   mkjps( indata = tdat, m = m, bigK = K, seed = sd, ntree = ntree )

    laout           <-   lapply( alphas, MarkovAl, jps = jps, K = K, m = m, 
                                 EmpEst = EmpEst, NEst = NEst )
    trtR            <-   do.call(rbind,laout)
    trtRL           <-   mkRLongM( trtR, K, trt=trt )

    nms             <-   c( paste0("Main",trt,"R"), paste0("Main",trt,"RL"), paste0("Main",trt,"wts"))
    Ret[[nms[1]]]   <-   trtR
    Ret[[nms[2]]]   <-   trtRL
    Ret[[nms[3]]]   <-   wts

    if ( returnJP ) {
       nm <- paste0("jps",trt)
       Ret[[nm]] <- jps
    }

    if ( NBootstraps > 0 ) {
      set.seed( seeds[trt] )
      llout <- lapply( bBS:eBS, oneSampM, jps=jps, trt=trt, m=m, nsamp = nr, K = K, 
                       sd = sd, ntree = ntree, EmpEst = EmpEst, NEst = NEst, 
                       alphas = alphas )

      SampR   <- lapply(llout,function(x) { return(x$SampR  ) } )
      SampRL  <- lapply(llout,function(x) { return(x$SampRL ) } )
      Sampwts <- lapply(llout,function(x) { return(x$wtsSamp) } )

      SampR   <- do.call(rbind,SampR)
      SampRL  <- do.call(rbind,SampRL)
      Sampwts <- do.call(rbind,Sampwts)

      nms            <-  c( paste0("Samp",trt,"R"),   paste0("Samp",trt,"RL"), 
                            paste0("Samp",trt,"wts"), paste0("Samp",trt))
      Ret[[nms[1]]]  <-  SampR
      Ret[[nms[2]]]  <-  SampRL
      Ret[[nms[3]]]  <-  Sampwts

      if ( returnSamples == TRUE ) {
         Samp   <- lapply(llout,function(x) { return(x$Samp  ) } )
         Samp   <- do.call(rbind,Samp)
         Ret[[nms[4]]] <- Samp
      }
    }
  }
  class(Ret) <- c("salbmM")
  return(Ret)
}
