EmpEst <- function( lst, alpha, NEst, m, K  ) {
   tab1         <- lst[[1]]
   p1           <- aggregate( f ~ y1, data=tab1, sum)
   samp         <- sample( p1[,"y1"], size=NEst, prob = p1[,"f"], replace = TRUE)
   samp         <- data.frame( y1 = samp )

   Mout         <- matrix(NA, nrow=2, ncol=K+2)
   Mout[1,1:3]  <- c(alpha,1,mean(samp[,"y1"]))
   Mout[2,1:3]  <- c(alpha,2,mean(samp[,"y1"]))
   cSum         <- samp[,"y1"]

   for ( j in 2:K ) {
     st    <-  max(1,j-m)
     nt    <-  min(m,j-1)

     nm    <-  paste("y",j,sep="")
     nml   <-  paste("y",st:(j-1),sep="")
     tab   <-  lst[[j]]
     tab   <-  tab[ tab[,nm] == 1, ]
     stmp  <-  seq( nt-1, 0, by=-1 )
     pow   <-  2^stmp
     loc   <-  as.matrix(samp[, nml, drop=FALSE ]) %*% matrix(pow,ncol=1) + 1

     p     <-  tab[ loc, "cf" ]
     y     <-  as.numeric( runif(NEst) <= p )
     samp[,nm] <- y

     if (j > m) samp <- samp[,-1]
     Mout[1,j+2] <- mean(y)
     cSum        <- cSum + y
     Mout[2,j+2] <- mean(cSum)
   }
   colnames(Mout) <- c("alpha","type",paste("t",1:K,sep=""))
   return(Mout)
} 
