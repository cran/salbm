MarkovAl <- function( alpha, jps, K, m, EmpEst = FALSE, NEst = 0 ) {
    outlst        <-   list()
    outf          <-   first( jps[[1]], alpha = alpha, m = m )
    outlst[[1]]   <-   outf

    out2          <-   second( jp = jps[[2]], t = 2, m = m, bigK = K, outf, alpha= alpha )
    outlst[[2]]   <-   out2

    for ( iter in 3:K ) {
      outt   <- second( jp = jps[[iter]], t = iter, m = m, bigK = K, outlst[[iter-1]]$Results, alpha= alpha )
      outlst[[iter]] <- outt
    }

    cflist <- list()
    for ( j in 1:K ) {
      if ( j == 1 ) {
        tmp           <-   outlst[[1]]
        nms           <-   paste("y",1:(m+2),sep="")
        tmp           <-   tmp[, c( nms, "f" )]
        tmpy          <-   tmp[,nms]
        indx          <-   do.call( order, tmpy )
        tmp           <-   tmp[ indx, ]
        cflist[[1]]   <-   tmp
      } else {
        tmp           <-   outlst[[j]]$CondResult
        st            <-   max(j - m, 1)
        en            <-   min( j, st + m )
        nms           <-   paste("y",st:en,sep="")
        sz            <-   2^length(nms)
        tmp           <-   tmp[, c(nms,"cf") ]
        if ( nrow(tmp) != sz ) {
            tplate            <- as.data.frame(mkPat2(length(nms)))
            tplate[]          <- lapply( tplate, factor )
            colnames(tplate)  <- nms
            tmp               <- merge(tplate,tmp,all.x=TRUE)
            tmp[ is.na(tmp[,"cf"]), "cf" ] <- 0
        }    
        tmpy          <-   tmp[, nms ]
        indx          <-   do.call( order, tmpy )
        tmp           <-   tmp[ indx, ]
        cflist[[j]]   <-   tmp
      }
    }

    if ( EmpEst == FALSE | NEst == 0 )  {
      FResults <- StdEst( outf = outf, lst = cflist, alpha = alpha, m = m, K = K ) 
    } else {
      cflist   <- dfac.list(cflist)
      FResults <- EmpEst( lst = cflist, alpha = alpha, NEst = NEst, m = m, K = K ) 
    }
    colnames(FResults) <- c("alpha","type",paste("t",1:K,sep=""))
    return(FResults)
}
