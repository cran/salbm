options(width=210,max.print=50)
library(salbm,lib.loc="../../../libs/")

base        <-   "LRDS/salbmResults"                  ## where data was stored
NP          <-   4                                    ## number of parts
x0          <-   readRDS( paste0( base, "0.rds" ) )   ## the baseline data

print(names(x0))

ggs <- function(part,name) {
  iname <- sprintf("%s_part%03d.rds", name, part)
  tsamp <- readRDS(iname)
  return(tsamp)
}  
Samps       <-   lapply(1:NP, ggs, name = base )      ## read the bootstrap sample results

M  <-  salbmCombine(x0=x0, Samps=Samps)
print(M)
saveRDS(M, "LRDS/M.rds")
