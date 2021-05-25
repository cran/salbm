options( width=200, max.print=10000 )

library(salbm,lib.loc="../../../libs")
# --------------------------------------

Res     <-  readRDS( "../Est/LRDS/M.rds" )
Res     <-  Res[[ "Res12" ]]
Res     <-  getContourPlot( Res, ci=c("lb4","ub4") )
ERD     <-  Res$ERD
SRD     <-  Res$SRD

tp      <-  1 
K       <-  Res$K
alphas  <-  Res$alphas

addDiagC <- function(x) {
  nr        <-  nrow(x)
  nc        <-  ncol(x)
  if ( nr < 1 | nc < 3 ) return(NULL)
 
  sn               <-  1*(x[,1] >= x[,2]) -  1*( x[,1] < x[,2] )
  rmp              <-  sn * abs(x[,1] - x[,2]) / sqrt(2)

  cc               <-  rep("#FFFFFFFF",nr)
  cc[ rmp < -1.4 ] <- "#AAAAAAFF"
  cc[ rmp <  0.0 ] <- "#555555FF"
  cc[ rmp <  1.4 ] <- "#000000FF"

  x$ramp           <- rmp
  x$color          <- cc
  return(x)
}
# --------------------------------------

Epts   <-  ERD[ sign(ERD[,4]) == sign(ERD[,5]), ]
Epts   <-  addDiagC( Epts )

Spts   <-  SRD[ sign(SRD[,4]) == sign(SRD[,5]), ]
Spts   <-  addDiagC( Spts )

ConNmE <-  paste("Plots/Econtour.pdf", sep = "" )
ConNmS <-  paste("Plots/Scontour.pdf", sep = "" )

# --------------------------------------

pdf(file=ConNmE, height=5, width=6.8)
par(mar=c(4,5,0.5,0.5),cex.axis=1.2,cex.lab=1.4)

filled.contour( x = alphas, y = alphas,
   z              = matrix(ERD[,3],length(alphas),length(alphas)),
   xlab           = expression(paste(alpha, " (Treatment 1)")),
   ylab           = expression(paste(alpha, " (Treatment 2)")),
   nlevels        = 8,
   color.palette  = colorRampPalette(c( "#993404","#D95F0E","#FE9929","#FFD9BE","#FFFFD4"), space="rgb"),
   plot.axis      = ( points( Epts[,c(1,2)], pch=15, cex=0.8, col = Epts[,"color"] ) )
   ) 
dev.off()

pdf(file=ConNmS, height=5, width=6.8)
par(mar=c(4,5,0.5,0.5),cex.axis=1.2,cex.lab=1.4)
filled.contour( x = alphas, y = alphas,
   z              = matrix(SRD[,3],length(alphas),length(alphas)),
   xlab           = expression(paste(alpha, " (Treatment 1)")),
   ylab           = expression(paste(alpha, " (Treatment 2)")),
   nlevels        = 8,
   color.palette  = colorRampPalette(c( "#993404","#D95F0E","#FE9929","#FFD9BE","#FFFFD4"), space="rgb"),
   plot.axis      = ( points( Spts[,c(1,2)], pch=15, cex=0.8, col = Spts[,"color"]) )
   ) 
dev.off()
# --------------------------------------
