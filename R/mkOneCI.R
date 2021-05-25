# ----------------------------------------------------------------
# Given two estimates (Est1,Est2) a pair of SEs and a pair of
# bootstrap standard error estimates, produce CI's for Est1, Est2
# and for the difference Est2 - Est1.
# cor is the bias-corrected estimate
# ----------------------------------------------------------------
mkOneCI <- function( Est1, SEM1, SESub1, Est2, SEM2, SESub2 ) {

  mark                <-     (( SESub1[,3] > 0 ) & ( SESub2[,3] > 0 ))
  SESub1              <-     SESub1[mark,]
  SESub2              <-     SESub2[mark,]

  forCI1              <-     prepCI( Est1, SEM1, SESub1 )
  forCI2              <-     prepCI( Est2, SEM2, SESub2 )

  Est3                <-     Est2 - Est1
  SEM3                <-     sqrt( SEM1^2 + SEM2^2 )
  SESub3              <-     SESub2
  SESub3[,2]          <-     SESub2[,2] - SESub1[,2]  
  SESub3[,3]          <-     sqrt(SESub2[,3]^2 + SESub1[,3]^2)

  forCI3              <-     prepCI( Est3, SEM3, SESub3 )

  cor1                <-     forCI1[1,"cor"]  
  cor2                <-     forCI2[1,"cor"]  
  cor3                <-     forCI3[1,"cor"]  

  CIs1                <-     c( Est1, SEM1, cor1, 1, incifn( forCI1 ))
  CIs2                <-     c( Est2, SEM2, cor2, 2, incifn( forCI2 ))
  CIs3                <-     c( Est3, SEM3, cor3, 0, incifn( forCI3 ))

  CIs                 <-     rbind( CIs1, CIs2, CIs3 )
  colnames(CIs)       <-     c("Est","SE","CEst","trt",paste( c("lb","ub"), sapply(1:6,rep,2), sep=""))
  return(CIs)
}
