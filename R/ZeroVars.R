# count bootstraps with no variance at each k
ZeroVars <- function(obj) {
   Narm         <-   obj$Narm
   K            <-   obj$K
   tlist        <-   c("E0", "E1", "Em", "S0", "S1", "Sm")
   wtNames      <-   unlist(lapply(paste0("Samp",1:Narm),function(x) paste0(x,"wts")))
   vnms         <-   paste0("T",1:K)

   subZeros <- list()
   for ( i in 1:Narm ) {
     nm  <- wtNames[i]
     W   <- obj[[nm]]
     W   <- W[ W[,"type"] %in% tlist, ] 
     Wz  <- apply((W[,vnms] == 0), 2, as.numeric )
     W   <- cbind(W[,"sub"],Wz)
     colnames(W) <- c("sub", vnms)
     z   <- lapply( 1:K, function(x){ aggregate( as.formula(paste( paste0("T",x), "~ sub")), data=W, max )} )
     z   <- do.call("cbind",z)
     z   <- z[,c("sub",vnms)]
     nm1 <- paste0("z", i )
     subZeros[[nm1]] <- z
     if ( i > 1 ) {
       nm1 <- paste0("zarm1", i )
       z1  <- subZeros[["z1"]]
       z1[,vnms] <- z1[,vnms] + z[,vnms]
       subZeros[[nm1]] <- z1
     } 
   }
   fn <- function(x) {
     apply(x[,vnms],2,function(y) sum(y>0))
   }
   ZTots <- lapply(subZeros,function(x) { apply(x[,vnms],2,function(y) sum(y>0)) })
   return( list( Zeros = subZeros, ZTots = ZTots ))
}
