# ---------------------------------------------------------------------
# for data set dat compute E_t the expected value of dat[,t] at each t,
# and S_t the expected value of the cummulent under the following
# conditions:
#  E0, S0   when 0 is substituted for missing values in dat,
#  E1, S1   when 1 is substituted for missing values in dat,
#  Em, Sm   when the mean value of dat[,t] is substituted for
#           dat[i,t] when dat[i,t] is missing.
#
# Means and standard devations are computed for each time t.
# ---------------------------------------------------------------------
wtsDat <- function( dat, sub, trt )  {
  dat[ dat == 2 ]        <-      NA
  nr                     <-      nrow(dat)
  nc                     <-      ncol(dat)

  dat0  <-  dat1  <- datm  <- dat
  
  dat0[ is.na(dat0) ]    <-      0
  dat1[ is.na(dat1) ]    <-      1
  datm[]                 <-      apply(datm,2, function(x) { mn <- mean(x,na.rm=TRUE); x[ is.na(x) ] <- mn; return(x)})

  cdat0                  <-      t(apply(dat0,1,cumsum))
  cdat1                  <-      t(apply(dat1,1,cumsum))
  cdatm                  <-      t(apply(datm,1,cumsum))

  E0                     <-      apply(dat0,2,sd) / sqrt(nr)
  S0                     <-      apply(cdat0,2,sd) / sqrt(nr)
  ME0                    <-      apply(dat0,2,mean)
  MS0                    <-      apply(cdat0,2,mean)
  
  E1                     <-      apply(dat1,2,sd) / sqrt(nr)
  S1                     <-      apply(cdat1,2,sd) / sqrt(nr)
  ME1                    <-      apply(dat1,2,mean)
  MS1                    <-      apply(cdat1,2,mean)
  
  Em                     <-      apply(datm,2,sd) / sqrt(nr)
  Sm                     <-      apply(cdatm,2,sd) / sqrt(nr)
  MEm                    <-      apply(datm,2,mean)
  MSm                    <-      apply(cdatm,2,mean)
                                       
  omat                   <-      matrix(c(E0,E1,Em,S0,S1,Sm,ME0,ME1,MEm,MS0,MS1,MSm),nrow=12,byrow=TRUE)
  omat                   <-      as.data.frame(omat)
  names(omat)            <-      paste("T",1:nc,sep="")
  omat[,"type"]          <-      c("E0","E1","Em","S0","S1","Sm","ME0","ME1","MEm","MS0","MS1","MSm")
  omat[,"trt"]           <-      trt
  omat[,"sub"]           <-      sub
  return(omat)
}
