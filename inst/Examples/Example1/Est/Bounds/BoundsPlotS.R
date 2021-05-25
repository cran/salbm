# --------------------------------------------------------------------------------
options( width=150, max.print=250 )
library(ggplot2)

R        <-   readRDS("LRDS/salbmResults0.rds")
Narm     <-   R$Narm  # 2 arms only one comparison 
K        <-   R$K
alphas   <-   R$alphas
mina     <-   min(alphas)
maxa     <-   max(alphas)

M1RL     <-   as.data.frame(R$Main1RL )
M1wts    <-   as.data.frame(R$Main1wts)
M2RL     <-   as.data.frame(R$Main2RL )
M2wts    <-   as.data.frame(R$Main2wts)

R1       <-   M1RL[ M1RL[,"type"]==2, c("alpha","k","type","Est")]
R2       <-   M2RL[ M2RL[,"type"]==2, c("alpha","k","type","Est")]

k       <-   K

b       <-   R1[ R1[,"k"] == k & R1[,"alpha"] ==  mina, "Est"]
loc1    <-   c( mina + 2, b )
b       <-   R2[ R2[,"k"] == k & R2[,"alpha"] ==  mina, "Est"]
loc2    <-   c( mina + 2, b )

loc     <- rbind(loc1,loc2)
loc     <- as.data.frame(loc)
names(loc) <- c("alpha","Est")
loc[,"txt"] <- c("trt = 1", "trt = 2")
rownames(loc) <- NULL

nplt <- function( dat, name, ylim = c(0,1), h=4.0, w=6, ylab,
                  col = c("#FF8866FF","#88AAEEFF","#55CC77FF") ) {

   alphas     <-  dat[,"alpha"]
   dat[,"colk"] <- as.character( dat[,"trt"] )
   theme_set(theme_minimal())

   pdf(file=name, height=h, width=w)

   h <- ggplot( dat, aes(x=alpha,y=Est,group=colk,color=colk)) +
        geom_line(size=1.2,color="#AAAAFFFF",alpha=0.8)+  
        geom_point(size=2.0,alpha=0.7) + 
        geom_text( data = loc, aes(x=alpha,y=Est,group=txt,label=txt),
                   hjust=0, vjust=2, color="#777777FF", size=4 ) +
        scale_x_continuous( name = expression(alpha),breaks=seq(mina,maxa,by=5)) +
        scale_y_continuous( name = eval(parse(text=ylab))) +
        scale_color_manual( values= col ) +
        coord_cartesian(xlim=c(mina,maxa), ylim = ylim) +
        theme(legend.position="none",
              axis.text.x=element_text(size=10),
              axis.text.y=element_text(size=10),
              axis.title.x=element_text(size=11,margin=margin(10,0,3,0)),
              axis.title.y=element_text(size=11,margin=margin(0,10,0,3)))
   print(h)
   dev.off()
   return(invisible())
}

ylab <-  "expression('E['~Sigma~Y[k]~'|'~alpha~']')"; 
col  <-  c("#FF2222FF","#2222FFFF","#55CC77FF","#FF8866FF","#88AAEEFF","#55CC77FF","#888888FF") 

R1[,"trt"] <- 1
R1 <- R1[order(R1[,"k"],R1[,"alpha"]),]
rownames(R1) <- NULL
R2[,"trt"] <- 2
R2 <- R2[order(R2[,"k"],R2[,"alpha"]),]
rownames(R2) <- NULL

dta <- rbind(R1,R2)
dta <- dta[ dta[,"k"] == K, ]

nplt( dat = dta , name = "Plots/BoundsPlotS.pdf", ylim = c(2.0,6.0), ylab = ylab,   col = col )

