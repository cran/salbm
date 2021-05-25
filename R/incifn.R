# --------------------------------------------------------
# Six CI's are produced.
# --------------------------------------------------------
incifn <- function( data ) {
  sampM         <-    mean( data[,"sampR"] )
  mainSE        <-    data[1,"mainSE"]

  # quantile and sym quantile 
  ECI1          <-    quantile( data[,"sampR"], c(0.025, 0.975) )
  Eqtile2       <-    quantile( abs(data[,"d"]), c(0.95) )
  ECI2          <-    c( sampM - Eqtile2, sampM + Eqtile2 )

  # t diff divided by sub se 
  Eqtile3       <-    quantile( data[,"t"], c(0.025, 0.975) )
  Eqtile3       <-    mainSE * Eqtile3
  ECI3          <-    c( sampM - Eqtile3[2], sampM - Eqtile3[1] )

  # sym t diff divided by sub se
  Eqtile4       <-    quantile( abs(data[,"t"]), c(0.95) )
  Eqtile4       <-    mainSE * Eqtile4
  ECI4          <-    c( sampM - Eqtile4, sampM + Eqtile4 )

  # bias corrected version
  # mu = 2mE - mean(sE)
  # (sE - mu)/sseE
  Eqtile7       <-    quantile( data[,"cort"], c(0.025, 0.975) )
  Eqtile7       <-    mainSE * Eqtile7
  ECI7          <-    c( sampM - Eqtile7[2], sampM - Eqtile7[1] )

  # symmetric version of bc
  Eqtile8       <-    quantile( abs(data[,"cort"]), c(0.95) )
  Eqtile8       <-    mainSE * Eqtile8
  ECI8          <-    c( sampM - Eqtile8, sampM + Eqtile8 )

  Ret           <-    c( ECI1, ECI2, ECI3, ECI4, ECI7, ECI8 )
  return(Ret)
}
