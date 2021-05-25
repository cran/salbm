options( width=150, max.print=1000 )

library(salbm,lib.loc="../../../libs")

R        <-   readRDS("../Est/LRDS/M.rds")
Narm     <-   R$Narm  # 2 arms so only one comparison
R        <-   R[[ "Res12" ]]

Res      <-   getCIPlot( R )

K        <-   Res$K
alphas   <-   Res$alphas
Xbnds    <-   range(alphas)   

trt1     <-   readRDS("../RDS/Eg1_1.rds")
trt2     <-   readRDS("../RDS/Eg1_2.rds")

trt1[ trt1==2 ] <- NA
trt2[ trt2==2 ] <- NA

d1 <- getEYnomoPlot( trt1[,K], Res$ER1[,2] )
d1 <- matrix( c( alphas, d1 ), ncol=2 )
colnames(d1) <- c("alpha","y")

d2 <- getEYnomoPlot( trt2[,K], Res$ER2[,2] )
d2 <- matrix( c( alphas, d2 ), ncol=2 )
colnames(d2) <- c("alpha","y")

xlim  <- range(alphas)
ylim  <- range( c( d1[,2], d2[,2] ) )
lloc  <- c(xlim[1], ylim[2] - 0.02 * ( ylim[2] - ylim[1] )) 
gpar  <- list(mar=c(4,5,1,1),mgp=c(2.5,0.6,0),cex=1.0,cex.lab=1.1,cex.axis=1.0)

x <- EYnomoPlot( Res=cbind(d1,d2[,2]), file="Plots/EYnomo.pdf", height=4.5, width=5, 
                 ylab = "E[Y|not obs] - E[Y|obs] as a\n percentage of E[Y|obs]", 
                 xlim = xlim, ylim = ylim , ltext = paste("Group",1:2), gpar=gpar,
                 col=c("#CC8866FF","#6688CCFF","#66CC88FF"))

