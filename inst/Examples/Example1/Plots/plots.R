# --------------------------------------------------------------------------------
options( width=150, max.print=250 )

library(salbm,lib.loc="../../../libs")

R        <-   readRDS("../Est/LRDS/M.rds")
Narm     <-   R$Narm  # 2 arms only one comparison 
R        <-   R[[ "Res12" ]]

Res      <-   getCIPlot( R, ci=c("lb2","ub2"))
K        <-   Res$K
alphas   <-   Res$alphas
Xlim     <-   range(alphas)   

Elim1    <-   climits(range(rbind(Res$ER1[,2:4],Res$ER2[,2:4])),oset=c(-0.01,0.0))
Elim2    <-   climits(range(Res$ERD[,2:4]),oset=c(-0.2,0.1))

Slim1    <-   climits(range(rbind(Res$SR1[,2:4],Res$SR2[,2:4])),oset=c(-0.01,0.1))
Slim2    <-   climits(range(Res$SRD[,2:4]),oset=c(-0.2,0.1))

Elab     <-   paste("Proportion outcome=1 at visit",K)
ElabD    <-   paste("\n Difference in proportions outcome=1\n at visit",K,"(group2 - group1)")

Slab     <-   paste("Number of visits outcome=1\n by visit",K)
SlabD    <-   paste("\n Difference in number of visits\n outcome=1 by visit",K,"(group2 - group1)")

mar      <-   c(4.0,5,0.5,0.5)
h        <-   4.0
w        <-   6.5

CIPlot(Res$ER1, "Plots/Trt1E.pdf",   h, w, Elab,   Xlim, Elim1, gpar=list(mar=mar))
CIPlot(Res$ER2, "Plots/Trt2E.pdf",   h, w, Elab,   Xlim, Elim1, gpar=list(mar=mar))
CIPlot(Res$ERD, "Plots/TrtDE.pdf",   h, w, ElabD,  Xlim, Elim2)

CIPlot(Res$SR1, "Plots/Trt1S.pdf",   h, w, Slab,   Xlim, Slim1)
CIPlot(Res$SR2, "Plots/Trt2S.pdf",   h, w, Slab,   Xlim, Slim1)
CIPlot(Res$SRD, "Plots/TrtDS.pdf",   h, w, SlabD,  Xlim, Slim2)
