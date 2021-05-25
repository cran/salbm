options(width=180,max.print=200,mc.cores=1,rf.cores=1)
suppressPackageStartupMessages(library(randomForestSRC,quietly=TRUE))

library(salbm,lib.loc="../../../libs/")
# ---------------------------------------------------------------
  args             <-   commandArgs(trailingOnly=TRUE)
  if ( length(args) < 3 ) stop("need at least 3 arguments\n")
  No               <-   as.numeric(args[1])
  NumB             <-   as.numeric(args[2])
  bBS              <-   as.numeric(args[3])
  seeds            <-   c(21+No, 481+No, 221+No)
  seeds2           <-   c(-53 - (No-1) * NumB, -388 - (No-1) * NumB, -271 - (No-1) * NumB )
  iname            <-   sprintf("LRDS/salbmResults0.rds")
  obj              <-   readRDS(iname)
  oname            <-   sprintf("LRDS/salbmResults_part%03d.rds", No )
  tm0              <-   proc.time()
# ---------------------------------------------------------------
set.seed(233+No)
Rup <- addSamples( obj = obj, 
            nseeds = seeds, nseeds2 = seeds2, bBS = bBS, 
            NBootstraps = NumB, ReturnJP = FALSE)
saveRDS(Rup,oname)
print(Rup)

tm1 <- proc.time()
print(tm0)
print(tm1)
print(tm1 - tm0)


