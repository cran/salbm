options(width=180,max.print=2000,mc.cores=1,rf.cores=1)
suppressPackageStartupMessages(library(randomForestSRC,quietly=TRUE))

library(salbm,lib.loc="../../../../libs/")
# ---------------------------------------------------------------
   No               <-   1  
   oname            <-   sprintf("LRDS/salbmResults0.rds")
   trt1             <-   readRDS("../../RDS/Eg2_1.rds")
   trt2             <-   readRDS("../../RDS/Eg2_2.rds")
   data             <-   list( trt1 = trt1, trt2 = trt2 )
   K                <-   ncol(trt1)
   tm0              <-   proc.time()
# ---------------------------------------------------------------
set.seed(231)
R <- salbmM( data = data , m=3, K = K , ntree = 1000, 
            seeds=c(23,484), seeds2=c(-5,-6), alphas = -20:20,
            bBS = 1, NBootstraps = 0)
saveRDS(R,oname)
print(R)

tm1 <- proc.time()
print(tm0)
print(tm1)
print(tm1 - tm0)
