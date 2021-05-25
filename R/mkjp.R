mkjp <- function( indata, m, bigK, seed = -1, ntree = 1000) {

  ## must have 2 <= 2*m+2 <= bigK
  data            <-   indata
  ncols           <-   ncol(data)
  nrows           <-   nrow(data)
  colnames        <-   names(data)

  jpout           <-  list()
  jpout[[1]]      <-  c( mean(data[,1]==0), mean(data[,1]==1), mean(data[,1]==2) )
  for ( j in 2:(ncols+2*m+1) ) {

    if ( j <= 2*m + 2 ) {
    
      inames      <-   colnames[1:(j-1)]
      anames      <-   colnames[1:j    ]
      TData       <-   data[,anames]
      f           <-   paste( colnames[j], "~", paste( inames, sep=" ", collapse="+") )
  
      rfout       <-    rfsrc( as.formula(f), data = TData, ntree  = ntree, seed = seed - j, sampsize = nrows,
                        samptype = "swr", ensemble = "all", bootstrap = "by.root" )

      ndata        <-   mkPatr3( j - 1 )
      ndata        <-   data.frame(ndata)
      names(ndata) <-   inames
      ndata[]      <-   lapply(ndata, function(x) factor(x, levels=c("0","1","2") ))
  
      pre          <-   predict(rfout,newdata=ndata)$predicted
      pre          <-   pre + 10^{-25}
      pre          <-   pre / apply(pre,1,sum)
      jpout[[j]]   <-   as.vector(jpout[[j-1]] * pre)
      
    } else if ( j <= bigK ) {

      st           <-   j - 2*m - 1
      inames       <-   colnames[st:(j-1)]
      anames       <-   colnames[st:j    ]
      TData        <-   data[,anames]
      f            <-   paste( colnames[j], "~", paste( inames, sep="", collapse="+"))
  
      rfout        <-   rfsrc( as.formula(f), data = TData, ntree  = ntree, seed = seed - j, sampsize = nrows,
                               samptype = "swr", ensemble = "all", bootstrap = "by.root" )

      ndata        <-   mkPatr3( 2*m + 1 )
      ndata        <-   data.frame(ndata)
      names(ndata) <-   inames
      ndata[]      <-   lapply(ndata, function(x) factor(x, levels=c("0","1","2") ))
  
      pre          <-   predict(rfout,newdata=ndata)$predicted
      pre          <-   pre + 10^{-25}
      pre          <-   pre / apply(pre,1,sum)
      prev         <-   jpout[[j-1]]
      v1           <-   seq(1,length(prev),by=3)
      prev         <-   prev[v1] + prev[v1+1] + prev[v1+2]
      jpout[[j]]   <-   as.vector(prev * pre)
    } else {
      prev         <-   jpout[[j-1]]
      v1           <-   seq(1,length(prev),by=3)
      prev         <-   prev[v1] + prev[v1+1] + prev[v1+2]
      jpout[[j]]   <-   as.vector(prev)
    }
  }
  return(jpout)
}
