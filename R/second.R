# ---------------------------------------------------------------
second <- function( jp, t = 2, m = 1, bigK = 2, prevRes, alpha=1 ) {
  st         <-    max( t - m,        1 )
  sp         <-    min( t + m + 1, bigK )
  yt         <-    paste("y",t,sep="")
  ytpmp1     <-    paste("y",t+m+1,sep="")
  mark1      <-    as.numeric( t <= bigK - m - 1 )
  mark2      <-    as.numeric( t <= m + 1        )

  tvs        <-    st:sp
  tvs        <-    tvs[ tvs != sp ]
  tvs        <-    tvs[ tvs !=  t ]
  bf         <-    tvs[ tvs <   t ]
  af         <-    tvs[ tvs >   t ] 
  
  strsb      <-    paste("y",tvs,sep="",collapse=" ")
  bothv      <-    paste("y",tvs,sep=""             )
  bothvpt    <-    c( bothv, yt                     )
  strsbf     <-    paste("y",tvs,sep="",collapse="+") 
  strsy      <-    paste("y", bf,sep="",collapse=" ")

  if ( length( af ) > 0 ) strso <- paste("y", af,sep="",collapse=" ")
  else strso <- " "

  yvecAll    <-   paste("y", st:sp,                         sep = "")
  yvec       <-   paste("y", st:(sp-1),                     sep = "")
  yvecmt     <-   paste("y", (st:(sp-1))[ st:(sp-1) != t ], sep = "")

  if ( t == bigK ) yvec <- paste("y",st:sp,sep="")
  
  yAllf      <-   paste( yvecAll, collapse = "+")
  yf         <-   paste( yvec,    collapse = "+")
  yfmt       <-   paste( yvecmt,  collapse = "+")
  
  if ( mark1 == 1 ) {
    strf          <-      paste("jp ~", strsbf, "+", yt)
    bynms         <-      paste("y", st:(sp-1), sep="")
    ords          <-   c( paste("y", st:(sp-1), sep=""),           "jp" )
    nms           <-   c( paste("y", st:(sp-1), sep=""),          "den" )
    snms1         <-   c( paste("y", st:(sp-1), sep=""),  ytpmp1,  "cf" )
    snms2         <-   c( paste("y", st:(sp-1), sep=""),  ytpmp1,   "f" )

    ## ------ aside ------
    Rprod  <-   rep(1,nrow=nrow(jp))
    for ( j in st:(t-1) ) {
      yvar        <-     paste("y",j,sep="")
      Rtmp        <-     as.numeric( 1 - (levels(jp[,yvar])[jp[,yvar]] == "2") )
      Rprod       <-     Rprod * Rtmp
    }
    jpr1          <-     jp[ Rprod == 1, ]
    sm            <-     sum(jpr1[,"jp"], na.rm=TRUE)
    jpr1[,"jp"]   <-     jpr1[,"jp"] / sm

    den           <-     aggregate( as.formula( strf ), data=jpr1, sum)
    colnames( den )[ colnames( den ) == "jp" ] <- "den" 

    jpr1          <-     merge( jpr1, den, by = bynms ) 
    jpr1[,"cf"]   <-     jpr1[,"jp"] / jpr1[,"den"]
    jpr1          <-     jpr1[,snms1]

    if ( mark2 == 1 ) Res <- prevRes
    else if ( mark2 == 0 ) {
      b  <- st
      e  <- sp-1
      if ( e > bigK ) e <- bigK
      ordf  <-  paste( "y", b:e, sep = "",collapse="+")
      Res   <-  prevRes
      Res   <-  aggregate( as.formula( paste( "f ~", ordf )), Res, sum )
    }
    
    inter         <-     merge( jpr1, Res, by = bynms )
    inter[,"f"]   <-     inter[,"cf"] * inter[,"f"]
    inter         <-     inter[,snms2]

  } else {
    if ( mark2 == 1 ) Res <- prevRes
    else if ( mark2 == 0 ) {
      b     <-  st
      e     <-  bigK
      ordf  <-  paste( "y", b:e, sep = "",collapse="+")
      Res   <-  prevRes
      Res   <-  aggregate( as.formula( paste( "f ~", ordf )), Res, sum )
    }
    inter <- Res
  }    
  
  ## Now R
  jp2             <-   inter
  R1              <-   as.numeric( 1 - (levels(jp2[,yt])[jp2[,yt]] == "2") )
  jp2             <-   cbind(jp2,R1)

  strf1           <-   paste(" wR1 ~", strsbf )
  strf2           <-   paste(" f ~",   strsbf )
  
  wR1             <-   R1 * jp2[,"f"]
  jp2             <-   cbind(jp2,wR1)
  wtR             <-   aggregate( as.formula( paste( " wR1 ~ ", yfmt ) ), data = jp2, sum)
  den             <-   aggregate( as.formula( paste( " f   ~ ", yfmt ) ), data = jp2, sum)
  colnames(den)[ colnames(den) == "f" ] <- "den" 
  wtR             <-   merge(wtR, den, by = yvecmt )
  wtR[,"wR1"]     <-   wtR[,"wR1"] / wtR[,"den"]
  wtR[,"wR0"]     <-   1 - wtR[,"wR1"]

  ## Exp: 
  jp3             <-   subset(jp2, R1 == 1, drop=TRUE)
  jp3             <-   droplevels(jp3)
  sm              <-   sum( jp3[,"f"],na.rm=TRUE)
  jp3[,"f"]       <-   jp3[,"f"] / sm

  ## tilting
  strf            <-   paste(" f      ~", strsbf, "+", yt)
  strf1           <-   paste(" wexpY1 ~", strsbf )
  strf2           <-   paste(" f      ~", strsbf )
  
  bynms           <-   bothv 

  ord             <-   paste(strsy,yt,strso,sep=" ")
  ord             <-   unlist(strsplit(ord," "))
  ord             <-   ord[ !(ord == "") ]

  jp4             <-   aggregate( as.formula(strf), data = jp3, sum)
  nvar            <-   as.numeric(levels( jp4[,yt] )[jp4[,yt]])
  jp4[, "expY1"]  <-   exp( alpha * nvar )
  jp4[,"wexpY1"]  <-   jp4[, "f" ] * jp4[,"expY1"]

  sm              <-   aggregate( as.formula( strf1 ), data = jp4, sum )
  dn              <-   aggregate( as.formula( strf2 ), data = jp4, sum )

  Eexp            <-   merge( sm, dn, by = yvecmt)
  Eexp[,"Eexp"]   <-   Eexp[,"wexpY1"] / Eexp[,"f"]
  
  Eexp            <-   Eexp[,c(yvecmt,"Eexp")]

  jp4             <-   merge(jp4,Eexp,by = yvecmt)
  jp4[,"expY1"]   <-   jp4[,"expY1"] / jp4[,"Eexp"]
  ExpTilt         <-   jp4[,c(yvec,"expY1")] 

  ## middle term

  mid             <-   merge( ExpTilt, wtR, by = yvecmt )
  mid[,"mid"]     <-   mid[,"wR1"] + mid[,"wR0"] * mid[,"expY1"]
  mid             <-   mid[,c(yvec,"mid")]

  ## f(y,o) part 3
  tst             <-   aggregate( as.formula( strf2 ), data = inter, sum)

  den             <-   aggregate( as.formula( paste(" f ~ ", yfmt ) ), data=jp3, sum )
  colnames(den)[ colnames(den) == "f" ] <- "den" 

  part1           <-   merge( jp3, den, by = bynms )
  part1[,"cond"]  <-   part1[, "f"] / part1[, "den"]

  ord2            <-   paste("y",st:sp,sep="")
  part1           <-   part1[,c(ord2,"cond")]

  join            <-   merge( part1, mid, by = ord )
  join            <-   merge( join, tst, by = bynms )
  join[,"result"] <-   join[,"cond"] * join[,"mid"] * join[,"f"]
  result          <-   join[,c(ord2,"result")]
  names(result)   <-   c(ord2,"f")

  sm              <-   sum(result[,"f"], na.rm=TRUE)
  result[,"f"]    <-   result[,"f"] / sm

  txtf            <-   paste("f ~",paste("y", st:t, sep="",collapse="+"),sep=" ")
  cresult         <-   aggregate( as.formula(txtf), data=result, sum )
  txtf1           <-   paste("f ~",paste("y",st:(t-1),sep="",collapse="+"),sep=" ")
  csum            <-   aggregate( as.formula(txtf1), data=cresult, sum )
  csum[,"csum"]   <-   csum[,"f"]
  nms             <-   paste("y",st:(t-1),sep="")
  csum            <-   csum[,c(nms,"csum")]

  cresult         <-   merge(cresult,csum,by=nms)
  cresult[,"cf"]  <-   cresult[,"f"] / cresult[,"csum"]
  cresult         <-   cresult[,c(nms,yt,"f","csum","cf")]

  Retlist         <-  list( Results = result, CondResults = cresult )
  return(Retlist)
}  
