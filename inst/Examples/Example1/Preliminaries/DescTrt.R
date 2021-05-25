options( width=150, max.print=1000 )

library(salbm,lib.loc="../../../libs")

Y1 <- readRDS("../RDS/Eg1_1.rds")
Y2 <- readRDS("../RDS/Eg1_2.rds")

Y1[ Y1 == 2 ] <- NA 
Y2[ Y2 == 2 ] <- NA 

# Dtable
Tmat1 <- PrepDescribeTrt( Y1 )
Tmat2 <- PrepDescribeTrt( Y2 )

sink("Tables/DescriptionTable.txt")
describeTrt( Tmat1, trtlab = "Treatment Group 1" )

describeTrt( Tmat2, trtlab = "Treatment Group 2" ) 
sink()


