# ---------------------------------------------------------------
first <- function( jp, alpha = 0, m = 1 ) {
  t              <- 1  
  yt             <- paste("y",t,sep="")
  ytpmp1         <- paste("y",t+m+1,sep="")
  bynms          <- paste("y",(t+1):(t+m),sep="")
  strsbf         <- paste(bynms,collapse="+")
  
  ## add R1 to data
  R1             <- ( 1 - (levels(jp[,yt])[jp[,yt]] == "2") )
  jp             <- cbind(jp,R1)
  
  wR1            <- R1 * jp[,"jp"]
  jp2            <- cbind(jp,wR1)
  wtR            <- aggregate( as.formula( paste( "wR1 ~", strsbf ) ), data = jp2, sum)
  den            <- aggregate( as.formula( paste( "jp  ~", strsbf ) ), data = jp2, sum)
  wtR            <- merge( wtR, den, by = bynms )
  wtR[,"wR1"]    <- wtR[,"wR1"] / wtR[,"jp"]
  wtR[,"wR0"]    <- 1 - wtR[,"wR1"]
  wtR            <- wtR[,c(bynms,"wR1","wR0")] 

  ## subset the data : reset probs
  jp3            <- subset(jp, R1 == 1, drop=TRUE)
  jp3            <- droplevels(jp3)
  sm             <- sum(jp3[,"jp"],na.rm=TRUE)
  jp3[,"jp"]     <- jp3[,"jp"] / sm

  ## tilting
  jp4            <- aggregate( as.formula( paste( "jp ~", yt, "+", strsbf) ), data = jp3, sum)
  nvar           <- as.numeric(levels( jp4[,yt] )[jp4[,yt]])
  jp4[,"expY1"]  <- exp( alpha * nvar )
  jp4[,"wexpY1"] <- jp4[, "jp" ] * jp4[,"expY1"]
  sm             <- aggregate( as.formula( paste( "wexpY1 ~", strsbf, sep="") ), data = jp4, sum )
  dn             <- aggregate( as.formula( paste( "jp     ~", strsbf, sep="") ), data = jp4, sum )
  Eexp           <- merge( sm, dn, by=bynms )
  Eexp[,"Eexp"]  <- Eexp[,"wexpY1"] / Eexp[,"jp"]
  Eexp           <- Eexp[,c(bynms,"Eexp")]
  jp4            <- merge(jp4,Eexp,by=bynms )
  jp4[,"expY1"]  <- jp4[,"expY1"] / jp4[,"Eexp"]
  ExpTilt        <- jp4[,c(yt,bynms,"expY1")] 

  ## middle term
  mid            <- merge( ExpTilt, wtR, by = bynms )
  mid[,"mid"]    <- mid[,"wR1"] + mid[,"wR0"] * mid[,"expY1"]
  mid            <- mid[,c(yt,bynms,"mid")]
             
  ## y1 y3 conditional on y2 and R1 == 1
  den            <- aggregate( as.formula( paste( "jp ~", strsbf ) ), data = jp3, sum )
  den            <- den[,c(bynms,"jp")]
  colnames(den)[ colnames(den) == "jp" ]  <- "den"
  cond           <- merge(jp3,den, by = bynms)
  cond[,"cond"]  <- cond[,"jp"] / cond[,"den"]
  cond           <- cond[,c(yt,bynms,ytpmp1,"cond")]

  part3          <- aggregate( as.formula( paste( "jp ~", strsbf ) ), data = jp, sum)

  result         <- merge( mid, part3, by = bynms)
  result         <- merge( cond, result, by = c(yt, bynms))
  result[,"f"]   <- result[,"cond"] * result[,"mid"] * result[,"jp"]
  result         <- result[,c(yt, bynms, ytpmp1,"f")]
  
  return(result)
}
