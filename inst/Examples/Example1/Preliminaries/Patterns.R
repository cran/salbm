## ------------------------------------------------
options( width=150, max.print=5000,as.is=TRUE, stringsAsFactors=FALSE)

library(salbm,lib.loc="../../../libs")

Y1  <- readRDS("../RDS/Eg1_1.rds")
Y2  <- readRDS("../RDS/Eg1_2.rds")
                 
Res1 <- PrepMissingPattern(Y=Y1)
sink("Tables/Missing1.txt")
missingPattern( TabR = Res1, trtlab = "Treatment 1" )
sink()

Res2 <- PrepMissingPattern(Y=Y2)
sink("Tables/Missing2.txt")
missingPattern( TabR = Res2, trtlab = "Treatment 2" )
sink()
