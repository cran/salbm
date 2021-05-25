options(width=180,max.print=100,mc.cores=1,rf.cores=1)

library(salbm,lib.loc="../../../libs/")

# ---------------------------------------------------------------
  M                <-  readRDS("../Est/LRDS/M.rds")
  M                <-  M[[ "Res12" ]]
  K                <-  M$K
  PrepTab          <-  PrepInferenceAssumption( M, CItp=4 )
  print(PrepTab)

  sink("Tables/InferenceAssumptions.txt")
  inferenceAssumption( PTab = PrepTab, TabName = " Table :", 
                 Title1txt = paste("Inference under various assumptions ( Symmetric CI, K =", K, ")"),
                 Title2Atxt = paste(" Expected value of Y_",K, sep=""),
                 Title2Btxt = paste(" Expected value of Sum_{i=1}^",K,"(Y_i)",sep=""),
                 w=8,d=4,s=14)
  sink()
# ---------------------------------------------------------------
