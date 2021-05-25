PrepMissingPattern <- function(Y)
{    
  K              <-     ncol(Y)
  b3             <-     3^( 0:(K-1) )

  Ym             <-     Y
  Ym[Ym==0]      <-     1
  Yb3            <-     as.matrix(Ym) %*% matrix(b3,ncol=1)
  tab            <-     table(Yb3)
  tab            <-     data.frame(tab)
  tab            <-     apply( as.matrix.noquote( tab ), 2, as.numeric)

  pcnt           <-     100* tab[,"Freq"] / sum(tab[,"Freq"])

  Mat            <-     matrix(NA,nrow=nrow(tab),ncol=K)
  dum            <-     tab[,1]
  for ( j in 1:K ) {
    Mat[,j]  <- dum %% 3
    dum      <- floor(dum/3)
  }
  colnames(Mat) <- paste("y",1:K,sep="")

  N1             <-     apply(Mat,1,function(x) sum( x == 1 ) )
  N2             <-     apply(Mat,1,function(x) sum( x == 2 ) )
  cnt            <-     cbind(N1,N2)

  f2             <-     apply(Mat,1,function(x) if ( any(x==2) ) { return(min( (1:K)[ x == 2 ] ))} else return(K+1) )
  l2             <-     apply(Mat,1,function(x) if ( any(x==2) ) { return(max( (1:K)[ x == 2 ] ))} else return(K+1) )

  mono           <-     rep(0,nrow(Mat))
  mono[N2 == 0]  <-     1
  mono[N2 > 0 & l2 == K & (l2 - f2 + 1 ) == N2 ] <- 1

  Mat            <-     cbind(Mat,tab,N1,N2,f2,l2,mono,pcnt)
  Mat            <-     as.data.frame(Mat)

  txt            <-     apply(as.matrix(Mat[,1:K]),1,function(x) paste(as.vector(x),sep="",collapse=""))
  txt            <-     gsub("1","*",txt)
  txt            <-     gsub("2","-",txt)

  Mat$txt        <-     txt

  ix             <-     order(-1*Mat[,"mono"],Mat[,"N2"])
  Mat            <-     Mat[ ix, ]

  st             <-     aggregate( Freq ~ mono+N1, data = Mat, sum )
  stp            <-     aggregate( pcnt ~ mono+N1, data = Mat, sum )

  names(st)      <-     c("mono","N1","SubFreq")
  names(stp)     <-     c("mono","N1","Subpcnt")

  sub            <-     merge(st,stp,by=c("mono","N1"))

  Mat            <-     merge(Mat,sub,by=c("mono","N1"))
  ix             <-     order(-1*Mat[,"mono"],Mat[,"N2"])
  Mat            <-     Mat[ ix, ]

  cnt   <- 1
  mk    <- 0 
  marks <- matrix(NA,nrow=nrow(Mat), ncol=2)
  for ( i in 1:nrow(Mat) ) {
    if ( i == 1 ) marks[cnt,] <- c(cnt,i)
    else if ( i == nrow(Mat) ) {
      cnt         <- cnt + 1
      marks[cnt,] <- c(cnt,i)
      }
    else if ( Mat[i-1,"mono"] != Mat[i,"mono"] ) {
      cnt         <- cnt + 1
      marks[cnt,] <- c(cnt,i-1)
      mk          <- 1
      }
    else if ( mk == 1 && ( Mat[i-1,"N2"] != Mat[i,"N2"]) ) {
      cnt         <- cnt + 1
      marks[cnt,] <- c(cnt,i-1)
    }
  }
  marks  <- marks[ !is.na(marks[,1]), ]

  tots        <- matrix(NA, nrow=nrow(marks), ncol=4 )
  tots[1,1:2] <- c(1,0)
  for ( j in 2:nrow(tots) ) {
     tots[j,1:2] <- marks[j,]
     tots[j,3:4] <- c( sum( Mat[ (tots[j-1,2]+1):(tots[j,2]), "Freq" ] ), sum( Mat[ (tots[j-1,2]+1):(tots[j,2]), "pcnt" ] ))
  }
  return(list( Mat = Mat, tots = tots, K = K ))
}
