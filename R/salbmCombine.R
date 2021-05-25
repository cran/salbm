## combine results into one
## x0 is the main results (with possible bootstraps)
## Samps contain results from bootstraps (with possible bootstraps)
salbmCombine <- function(x0, Samps=NULL, div=c(NA,NA)) {

  x0Names     <-   names(x0)       ## whats in the baseline data
  Narm        <-   x0$Narm         ## number of arms (usually 2)
  NB          <-   x0$NBootstraps  ## number of bootstraps in the baseline data (maybe 0)

  mna         <-   x0[[ "mna" ]]
  mxa         <-   x0[[ "mxa" ]]
  ndiv        <-   c(mna,mxa)

  if ( is.null(div)  ) div <- ndiv
  else if ( length(div) < 2 ) div <- ndiv
  else if ( any(is.na(div)) ) div <- ndiv
  else div <- c(min(div[1],mna,na.rm=TRUE),max(div[2],mxa,na.rm=TRUE))

  Lw          <-   c("R","RL","wts")
  mainNames   <-   unlist(lapply(paste0("Main",1:Narm),function(x) paste0(x,Lw) ))
  sampNames   <-   unlist(lapply(paste0("Samp",1:Narm),function(x) paste0(x,Lw) ))
  othNames    <-   c("K","alphas","Narm","ntree")
  if ( attributes(x0)[["class"]] == "salbmM" ) {
     othNames <- c(othNames,"m","EmpEst","NEst")
  } else {
    MainTiltmin   <-   x0[ paste0("MainTilt",1:Narm,"min") ]
    MainTiltmax   <-   x0[ paste0("MainTilt",1:Narm,"max") ]
    zero          <-   x0[ paste0("tiltzero",1:Narm) ]
    zero          <-   do.call(rbind,zero)
    STzNames      <-   paste0("Samp",1:Narm,"TiltZero")
    sampNames     <-   c(sampNames,STzNames)
  }

  selNames    <-   c(mainNames,othNames)
  inx0        <-   (selNames %in% x0Names)
  selNames    <-   selNames[ inx0 ]
  M           <-   x0[selNames]      ## select baseline results

  ## add bootstrap results
  for ( j in 1:length(sampNames) ) {
    nm  <-  sampNames[j]
    if ( is.null(Samps) & NB > 0 ) {
      X <- x0[[nm]]
      M[[nm]] <- X
    } else {
      X <- lapply(Samps, '[[', nm )  ## ']]'
      X <- do.call(rbind,X)
      if ( NB > 0 ) {
        Y <- x0[[nm]]
        X <- rbind(X,Y)
      }
      M[[nm]] <- X
    }
  }

  ## add samples if any
  sampleNames   <-   paste0("Samp",1:Narm)
  inx0          <-   ( sampleNames %in% x0Names )
  for ( j in 1:length(sampleNames) ) {
    nm  <-  sampleNames[j]
    X <- lapply(Samps, '[[', nm )  ## ']]'
    X <- do.call(rbind,X)
    if ( inx0[j] == TRUE ) {
       Y <- x0[[ nm ]]
       X <- rbind(Y,X)
    }  
    if ( !is.null(X) ) {
      if ( nrow(X) > 0 ) M[[nm]] <- X
    }
  }
  Z <- ZeroVars(M)
  M <- salbmGetCI( obj = M, div = div )
  M[[ "ZVars" ]] <- Z
  if ( attributes(x0)[["class"]] == "salbmM" ) {
     othNames <- c("m","EmpEst","NEst")
     M[othNames] <- x0[othNames]
  } else {
    zero      <-   x0[ paste0("tiltzero",1:Narm) ]
    zero      <-   do.call(rbind,zero)
    M[[ "zero"  ]] <- zero
    M[[ "MainTiltMin" ]] <- MainTiltmin
    M[[ "MainTiltMax" ]] <- MainTiltmax
  }
  return(M)
}
