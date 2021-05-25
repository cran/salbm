options(width=210,max.print=500,mc.cores=1,rf.cores=1)
suppressPackageStartupMessages(library(randomForestSRC,quietly=TRUE))

library(salbm,lib.loc="../../../libs/")
  oname     <-   "LRDS/salbmResults0.rds"
  trt1      <-   readRDS("../RDS/Eg1_1.rds")
  trt2      <-   readRDS("../RDS/Eg1_2.rds")
  K         <-   ncol(trt1)
  data      <-   list( trt1 = trt1, trt2 = trt2 )
  tm0       <-   proc.time()
# ---------------------------------------------------------------
set.seed(123)
R <- salbm( data = data, K = K, ntree = 1000,
            seeds = c(25,401),seeds2 = c(-3,-4), alphas = -12:8, 
            bBS = 1, NBootstraps = 0 )
saveRDS(R,oname)
print(R)

tm1 <- proc.time()
print(tm0)
print(tm1)
print(tm1 - tm0)
