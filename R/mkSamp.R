mkSamp <- function( this.jp, trt=1, m=3, nsamp, K ) { 
  k         <-   1
  lt        <-   k + 1 + 2*m
  ltm1      <-   lt - 1
  yvars     <-   paste("y",k:lt,sep="")

  jp0       <-   this.jp[[m+1]]
  indx      <-   sample( 1:nrow(jp0), nsamp, prob = jp0[,"jp"], replace = TRUE )
  samp0     <-   jp0[indx,yvars]

  # --------------------------------------------------

  if ( lt == K ) {
    rnd    <- runif( nrow( samp0 ) )
    samp0  <- samp0[ order(rnd), ]
    rownames(samp0) <- NULL
    return(samp0)
  }

  oindx     <-   do.call( order, samp0 )
  samp0     <-   samp0[ oindx, ]
  row.names(samp0) <- NULL

  # --------------------------------------------------

  for ( k in 2:(K - 1 - 2*m ) ) {
     lt      <-   k + 1 + 2*m
     ltm1    <-   lt - 1

     ylt     <-   paste( "y",     lt, sep="")
     byv     <-   paste( "y", k:ltm1, sep="")
     ylist   <-   paste( "y",   1:lt, sep="")

     jp1     <-   this.jp[[k+m]]
     tmp0    <-   jp1[ jp1[,ylt] == 0, ]
     tmp1    <-   jp1[ jp1[,ylt] == 1, ]
     tmp2    <-   jp1[ jp1[,ylt] == 2, ]

     colnames(tmp0)[colnames(tmp0)=="jp"] <- "jp0"
     colnames(tmp1)[colnames(tmp1)=="jp"] <- "jp1"
     colnames(tmp2)[colnames(tmp2)=="jp"] <- "jp2"

     cond <- merge( tmp0, tmp1, by = byv )
     cond <- merge( cond, tmp2, by = byv )
     sm   <- apply( cond[,c("jp0","jp1","jp2")], 1, sum )

     cond[,"jp0"]  <-  cond[,"jp0"] / sm
     cond[,"jp1"]  <-  cond[,"jp1"] / sm
     cond[,"jp2"]  <-  cond[,"jp2"] / sm

     cond1 <- merge( samp0, cond, by = byv, all.x = TRUE, all.y = FALSE )

     rnd   <- runif(  nrow( cond1 ) )
     ytmp  <- rep( 2, nrow( cond1 ) )

     ytmp[ rnd <= (cond1[,"jp0"] + cond1[,"jp1"]) ] <- 1
     ytmp[ rnd <=  cond1[,"jp0"] ] <- 0

     cond1[, ylt] <- ytmp
     cond1        <- cond1[ !is.na(cond1[,ylt]), ] 
     samp0        <- cond1[, ylist ]
  }
  ## random order?
  rnd    <- runif( nrow( samp0 ) )
  samp0  <- samp0[ order(rnd), ]
  rownames(samp0) <- NULL
  return(samp0)
}
