StdEst <- function( outf, lst, alpha, m, K ) {
    p1                <- aggregate( f ~ y1, data = outf, sum)
    p1[,"cf1"]        <- p1[,"f"]
    p1                <- p1[,c("y1","cf1")]

    prod              <- p1
    prod[,"prod"]     <- p1[,"cf1"]
    names(prod)       <- c("y1","cf1", "prod")

    FResults          <- matrix( NA, nrow=2, ncol=2+K)
    ty                <- as.numeric( levels( prod[,"y1"] ) )[prod[,"y1"]]
    FResults[1,1:3]   <- c( alpha, 1, weighted.mean(ty,w=prod[,"prod"]))
    FResults[2,1:3]   <- c( alpha, 2, weighted.mean(ty,w=prod[,"prod"]))

    r1                <- c(0,1)
    for ( j in 2:K ) {
      st                   <-   max( j - m, 1 )
      pjcf                 <-   lst[[j]]
      pjcfj0               <-   pjcf[ pjcf[,paste("y",j,sep="")] == 0, "cf" ]
      pjcfj1               <-   pjcf[ pjcf[,paste("y",j,sep="")] == 1, "cf" ]

      if ( j == 2 ) {
        nextt1             <-   prod[,"prod"] * pjcfj0
        nextt2             <-   prod[,"prod"] * pjcfj1
        nextb              <-   as.vector(t(cbind(nextt1,nextt2)))

        r                  <-   rep( c(0,1), length(nextb)/2 )
        FResults[1,j+2]    <-   sum(r * nextb,na.rm=TRUE) / sum(nextb,na.rm=TRUE)

        sm1                <-   sapply( r1, function(x) rep(x,2) ) + r
        FResults[2,j+2]    <-   sum(sm1 * nextb,na.rm=TRUE) / sum(nextb,na.rm=TRUE)
      } else {
        nextt1             <-   nextb * pjcfj0
        nextt2             <-   nextb * pjcfj1
        nextb              <-   as.vector(t(cbind(nextt1,nextt2)))

        r                  <-   rep( c(0,1), length(nextb)/2 )
        FResults[1,j+2]    <-   sum(r * nextb,na.rm=TRUE) / sum(nextb,na.rm=TRUE)

        sm1                <-   sapply( sm1, function(x) rep(x,2) ) + r
        FResults[2,j+2]    <-   sum(sm1 * nextb,na.rm=TRUE) / sum(nextb,na.rm=TRUE)
      }
    }
  colnames(FResults) <- c("alpha","type",paste("t",1:K,sep=""))
  return(FResults)
}
