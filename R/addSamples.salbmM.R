addSamples.salbmM <- function( obj, NBootstraps = 0, bBS = 1, 
                               nseeds = c(5,9), nseeds2=c(-4,-5), 
                               returnJP = TRUE, returnSamples = FALSE, ... )
{
  Narm <- obj$Narm
  ns  <- length(nseeds)
  ns2 <- length(nseeds2)
  if ( ns  < Narm ) nseeds  <- c( nseeds,  nseeds [ns ] + 1:(Narm-ns ))
  if ( ns2 < Narm ) nseeds2 <- c( nseeds2, nseeds2[ns2] - 1:(Narm-ns2))
  eBS <- bBS + NBootstraps - 1

  Ret  <-  obj
  if ( is.null(Ret$Upd) ) { 
    Ret$Nupd      <- 1 
    Ret$Upd       <- list( Upd1 = list( nseeds = nseeds, nseeds2 = nseeds2, bBS = bBS, eBS = eBS, NBootstraps = NBootstraps ))
  } else {
    Nupd          <- Ret$Nupd + 1 
    nm            <- paste0("Upd",Nupd)
    Ret$Nupd      <- Nupd  
    Ret$Upd[[nm]] <- list( nseeds = nseeds, nseeds2 = nseeds2, bBS = bBS, eBS = eBS, NBootstraps = NBootstraps )
  }

  K               <-   obj$K
  m               <-   obj$m
  EmpEst          <-   obj$EmpEst
  NEst            <-   obj$NEst
  if ( is.null(EmpEst) ) EmpEst <- FALSE
  if ( is.null(NEst)   ) NEst   <- 0
  alphas          <-   obj$alphas
  ntree           <-   obj$ntree
  
  data <- obj$data
  for ( trt in 1:Narm ) {
    sd              <-   nseeds2[trt]
    tdat            <-   data[[trt]]
    nr              <-   nrow(tdat)

    tjp             <-   obj[[ paste0("jps",trt) ]]

    if ( !returnJP ) Ret[[ paste0("jps",trt) ]] <-  NULL

    if ( NBootstraps > 0 ) {
      set.seed( nseeds[trt] )

      llout <- lapply( bBS:eBS, oneSampM, 
                       jps=tjp, trt=trt, m=m, nsamp = nr, K = K, 
                       sd = sd, ntree = ntree, EmpEst = EmpEst, NEst = NEst, 
                       alphas = alphas )

      SampR    <- lapply(llout,function(x) { return(x$SampR  ) } )
      SampRL   <- lapply(llout,function(x) { return(x$SampRL ) } )
      Sampwts  <- lapply(llout,function(x) { return(x$wtsSamp) } )

      SampR    <- do.call(rbind,SampR)
      SampRL   <- do.call(rbind,SampRL)
      Sampwts  <- do.call(rbind,Sampwts)

      nms     <- c( paste0("Samp",trt,"R"),    paste0("Samp",trt,"RL"),
                    paste0("Samp",trt,"wts"),  paste0("Samp",trt))

      if (!is.null(obj[[nms[1]]])) SampR   <- rbind( obj[[nms[1]]],   SampR)
      if (!is.null(obj[[nms[2]]])) SampRL  <- rbind( obj[[nms[2]]],  SampRL)
      if (!is.null(obj[[nms[3]]])) Sampwts <- rbind( obj[[nms[3]]], Sampwts)

      Ret[[nms[1]]]  <-  SampR
      Ret[[nms[2]]]  <-  SampRL
      Ret[[nms[3]]]  <-  Sampwts

      if ( returnSamples == TRUE ) {
        Samp   <- lapply(llout,function(x) { return(x$Samp ) } )
        Samp   <- do.call(rbind,Samp)
        Ret[[nms[4]]] <- Samp
      } 

    }
  }
# --------------------------------------------------------------------------------
  class(Ret) <- "salbmM"
  return(Ret)
}
