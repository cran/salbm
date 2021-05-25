options( width=200, max.print=10000 )

library(salbm,lib.loc="../../../libs")
source("colDot.R")
# --------------------------------------

Res     <-  readRDS( "../Est/LRDS/M.rds" )
Res     <-  Res[[ "Res12" ]]
Res     <-  getContourPlot( Res, ci=c("lb2","ub2") )
ERD     <-  Res$ERD
SRD     <-  Res$SRD

tp      <-  1 
K       <-  Res$K
alphas  <-  Res$alphas

# --------------------------------------

qE      <-  as.vector(quantile(ERD[,3],c(.1,.2,.3,.4,.5,.6,.7,.8,.9)))
ColMatE <-  ERD[ sign(ERD[,"lb2"]) == sign(ERD[,"ub2"]),]
colE    <-  colDot(x=ColMatE[,3],q=qE)

qS      <-  as.vector(quantile(SRD[,3],c(.1,.2,.3,.4,.5,.6,.7,.8,.9)))
ColMatS <-  SRD[ sign(SRD[,"lb2"]) == sign(SRD[,"ub2"]),]
colS    <-  colDot(x=ColMatS[,3],q=qS)


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
   plot.axis      = ( points( ERD[ sign(ERD[,"lb2"]) == sign(ERD[,"ub2"]), c("alpha1","alpha2")], pch=15, cex=1.0, col = colE))
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
   plot.axis      = ( points( SRD[ sign(SRD[,"lb2"]) == sign(SRD[,"ub2"]), c("alpha1","alpha2")], pch=15, cex=1.0, col = colS))
   ) 
dev.off()
# --------------------------------------
