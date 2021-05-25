PrepInferenceAssumption <- function( M, CItp=2, qt = c(0.025,0.975), SampCIType=1, carm = 2 ) {

   K                <-   M$K             
   nm2              <-   c( paste0( "Main",carm,"wts" ),  paste0( "Samp",carm,"wts" ) )
   Mwts1            <-   M$Main1wts      ## main
   Mwts2            <-   M[[ nm2[1] ]]
   Swts1            <-   M$Samp1wts      ## bootstraps
   Swts2            <-   M[[ nm2[2] ]]

   nm               <-   paste("T",K,sep="")
   meannms          <-   c("MEm","ME0","ME1","MSm","MS0","MS1")
   sdnms            <-   c("Em","E0","E1","Sm","S0","S1")

   Mmean1           <-   Mwts1[ Mwts1[,"type"] %in% meannms, c(nm,"type") ]
   Mmean2           <-   Mwts2[ Mwts2[,"type"] %in% meannms, c(nm,"type") ]

   names(Mmean1)    <-   c("M1","type")
   names(Mmean2)    <-   c("M2","type")

   Msd1             <-   Mwts1[ Mwts1[,"type"] %in%   sdnms, c(nm,"type") ]
   Msd2             <-   Mwts2[ Mwts2[,"type"] %in%   sdnms, c(nm,"type") ]

   names(Msd1)      <-   c("Msd1","type")
   names(Msd2)      <-   c("Msd2","type")

   Msd1[,"type"]    <-   paste0("M",Msd1[,"type"])
   Msd2[,"type"]    <-   paste0("M",Msd2[,"type"])


   Smean1           <-   Swts1[ Swts1[,"type"] %in% meannms, c(nm,"type","sub") ] 
   Smean2           <-   Swts2[ Swts2[,"type"] %in% meannms, c(nm,"type","sub") ] 

   names(Smean1)    <-   c("S1","type","sub")
   names(Smean2)    <-   c("S2","type","sub")


   Ssd1             <-   Swts1[ Swts1[,"type"] %in%   sdnms, c(nm,"type","sub") ] 
   Ssd2             <-   Swts2[ Swts2[,"type"] %in%   sdnms, c(nm,"type","sub") ] 

   names(Ssd1)      <-   c("Ssd1","type","sub")
   names(Ssd2)      <-   c("Ssd2","type","sub")

   Ssd1[,"type"]    <-   paste0("M",Ssd1[,"type"])
   Ssd2[,"type"]    <-   paste0("M",Ssd2[,"type"])

   T1               <-   merge(Mmean1,Msd1,by=c("type"))
   T11              <-   merge(Smean1,Ssd1,by=c("type","sub"))
   T1               <-   merge(T1,T11,by=c("type"))

   T2               <-   merge(Mmean2,Msd2,by=c("type"))
   T22              <-   merge(Smean2,Ssd2,by=c("type","sub"))
   T2               <-   merge(T2,T22,,by=c("type"))
   All              <-   merge(T1,T2,by=c("type","sub"))

   All$Di1          <-   All$S1 - All$M1
   All$ti1          <-   (All$S1 - All$M1)/All$Ssd1

   All$Di2          <-   All$S2 - All$M2
   All$ti2          <-   (All$S2 - All$M2)/All$Ssd2

   All$Md           <-   All$M2 - All$M1
   All$Mdi          <-   ( All$S2 - All$M2) - ( All$S1 - All$M1 )
   All$Mtdi         <-   (( All$S2 - All$M2) - ( All$S1 - All$M1 )) / sqrt( All$Ssd2^2 + All$Ssd1^2 )

   All$Sd           <-   All$S2 - All$S1

## type 1
   if (SampCIType == 1) { 
   agg1             <-   aggregate( M1 ~ type, data = All, mean )
   agg2             <-   aggregate( M2 ~ type, data = All, mean )
   aggd             <-   aggregate( Md ~ type, data = All, mean )

   qagg1            <-   aggregate( S1 ~ type, data = All, function(x) {quantile(x,qt)} )
   qagg2            <-   aggregate( S2 ~ type, data = All, function(x) {quantile(x,qt)} )
   qaggd            <-   aggregate( Sd ~ type, data = All, function(x) {quantile(x,qt)} )

   agg1             <-   merge(agg1,qagg1,by=c("type"))
   agg2             <-   merge(agg2,qagg2,by=c("type"))
   aggd             <-   merge(aggd,qaggd,by=c("type"))

   tab3             <-   merge(agg1,agg2,by=c("type"))
   tab3             <-   merge(tab3,aggd,by=c("type"))
   tab3             <-   as.data.frame( as.list(tab3) )
   stab <- tab3
} else if (SampCIType==2) {
## type 2
   agg1             <-   aggregate( M1  ~ type, data = All, mean )
   agg2             <-   aggregate( M2  ~ type, data = All, mean )
   aggd             <-   aggregate( Md  ~ type, data = All, mean )

   sagg1            <-   aggregate( S1  ~ type, data = All, mean )
   sagg2            <-   aggregate( S2  ~ type, data = All, mean )
   saggd            <-   aggregate( Sd  ~ type, data = All, mean )

   qagg1            <-   aggregate( Di1 ~ type, data = All, function(x) {quantile(x,qt)} )
   qagg2            <-   aggregate( Di2 ~ type, data = All, function(x) {quantile(x,qt)} )
   qaggd            <-   aggregate( Mdi ~ type, data = All, function(x) {quantile(x,qt)} )

   agg1             <-   merge(agg1,sagg1,by=c("type"))
   agg1             <-   merge(agg1,qagg1,by=c("type"))
   agg2             <-   merge(agg2,sagg2,by=c("type"))
   agg2             <-   merge(agg2,qagg2,by=c("type"))
   aggd             <-   merge(aggd,saggd,by=c("type"))
   aggd             <-   merge(aggd,qaggd,by=c("type"))

   tmp32            <-   merge(agg1,agg2,by=c("type"))
   tmp32            <-   merge(tmp32,aggd,by=c("type"))
   tmp32            <-   as.data.frame( as.list(tmp32))
   names(tmp32)     <-   c("type","M1","S1","lb1","ub1","M2","S2","lb2","ub2","Md","Sd","lbd","ubd")

   tab32            <-   tmp32
   tab32[,"lb1"]    <-   tmp32[,"S1"] - tmp32[,"ub1"]
   tab32[,"ub1"]    <-   tmp32[,"S1"] - tmp32[,"lb1"]
   tab32[,"lb2"]    <-   tmp32[,"S2"] - tmp32[,"ub2"]
   tab32[,"ub2"]    <-   tmp32[,"S2"] - tmp32[,"lb2"]
   tab32[,"lbd"]    <-   tmp32[,"Sd"] - tmp32[,"ubd"]
   tab32[,"ubd"]    <-   tmp32[,"Sd"] - tmp32[,"lbd"]

   tab32            <-   tab32[,c("type","M1","lb1","ub1","M2","lb2","ub2","Md","lbd","ubd")]

   stab <- tab32
} else if (SampCIType==3) {
## type 3
   agg1             <-   aggregate( M1  ~ type, data = All, mean )
   agg2             <-   aggregate( M2  ~ type, data = All, mean )
   aggd             <-   aggregate( Md  ~ type, data = All, mean )

   sagg1            <-   aggregate( S1  ~ type, data = All, mean )
   sagg2            <-   aggregate( S2  ~ type, data = All, mean )
   saggd            <-   aggregate( Sd  ~ type, data = All, mean )

   qagg1            <-   aggregate( abs(Di1) ~ type, data = All, function(x) {quantile(x,0.95)} )
   qagg2            <-   aggregate( abs(Di2) ~ type, data = All, function(x) {quantile(x,0.95)} )
   qaggd            <-   aggregate( abs(Mdi) ~ type, data = All, function(x) {quantile(x,0.95)} )

   agg1             <-   merge(agg1,sagg1,by=c("type"))
   agg1             <-   merge(agg1,qagg1,by=c("type"))
   agg2             <-   merge(agg2,sagg2,by=c("type"))
   agg2             <-   merge(agg2,qagg2,by=c("type"))
   aggd             <-   merge(aggd,saggd,by=c("type"))
   aggd             <-   merge(aggd,qaggd,by=c("type"))

   tmp33            <-   merge(agg1,agg2,by=c("type"))
   tmp33            <-   merge(tmp33,aggd,by=c("type"))
   tmp33            <-   as.data.frame( as.list(tmp33))
   names(tmp33)     <-   c("type","M1","S1","q1","M2","S2","q2","Md","Sd","qd")

   tab33            <-   tmp33
   tab33[,"lb1"]    <-   tmp33[,"S1"] - tmp33[,"q1"]
   tab33[,"ub1"]    <-   tmp33[,"S1"] + tmp33[,"q1"]
   tab33[,"lb2"]    <-   tmp33[,"S2"] - tmp33[,"q2"]
   tab33[,"ub2"]    <-   tmp33[,"S2"] + tmp33[,"q2"]
   tab33[,"lbd"]    <-   tmp33[,"Sd"] - tmp33[,"qd"]
   tab33[,"ubd"]    <-   tmp33[,"Sd"] + tmp33[,"qd"]

   tab33            <-   tab33[,c("type","M1","lb1","ub1","M2","lb2","ub2","Md","lbd","ubd")]

   stab <- tab33
} else if (SampCIType==4) {
## type 4
   agg1             <-   aggregate( M1  ~ type, data = All, mean )
   agg2             <-   aggregate( M2  ~ type, data = All, mean )
   aggd             <-   aggregate( Md  ~ type, data = All, mean )

   sdagg1           <-   aggregate( Msd1 ~ type, data = All, mean )
   sdagg2           <-   aggregate( Msd2 ~ type, data = All, mean )

   sagg1            <-   aggregate( S1  ~ type, data = All, mean )
   sagg2            <-   aggregate( S2  ~ type, data = All, mean )
   saggd            <-   aggregate( Sd  ~ type, data = All, mean )

   qagg1            <-   aggregate( ti1  ~ type, data = All, function(x) {quantile(x,qt)} )
   qagg2            <-   aggregate( ti2  ~ type, data = All, function(x) {quantile(x,qt)} )
   qaggd            <-   aggregate( Mtdi ~ type, data = All, function(x) {quantile(x,qt)} )

   agg1             <-   merge(agg1,sagg1,by=c("type"))
   agg1             <-   merge(agg1,sdagg1,by=c("type"))
   agg1             <-   merge(agg1,qagg1,by=c("type"))
   agg2             <-   merge(agg2,sagg2,by=c("type"))
   agg2             <-   merge(agg2,sdagg2,by=c("type"))
   agg2             <-   merge(agg2,qagg2,by=c("type"))
   aggd             <-   merge(aggd,saggd,by=c("type"))
   aggd             <-   merge(aggd,qaggd,by=c("type"))

   tmp44            <-   merge(agg1,agg2,by=c("type"))
   tmp44            <-   merge(tmp44,aggd,by=c("type"))
   tmp44            <-   as.data.frame( as.list(tmp44))
   names(tmp44)     <-   c("type","M1","S1","sd1","lb1","ub1","M2","S2","sd2","lb2","ub2","Md","Sd","lbd","ubd")

   tmp44$sdd        <-   sqrt( tmp44[,"sd1"]^2 + tmp44[,"sd2"]^2 )
   tab44            <-   tmp44
   tab44[,"lb1"]    <-   tmp44[,"S1"] - tmp44[,"ub1"] * tmp44[,"sd1"]
   tab44[,"ub1"]    <-   tmp44[,"S1"] - tmp44[,"lb1"] * tmp44[,"sd1"]
   tab44[,"lb2"]    <-   tmp44[,"S2"] - tmp44[,"ub2"] * tmp44[,"sd2"]
   tab44[,"ub2"]    <-   tmp44[,"S2"] - tmp44[,"lb2"] * tmp44[,"sd2"]
   tab44[,"lbd"]    <-   tmp44[,"Sd"] - tmp44[,"ubd"] * tmp44[,"sdd"]
   tab44[,"ubd"]    <-   tmp44[,"Sd"] - tmp44[,"lbd"] * tmp44[,"sdd"]


   tab44            <-   tab44[,c("type","M1","lb1","ub1","M2","lb2","ub2","Md","lbd","ubd")]

   stab <- tab44
} else if (SampCIType==5) {
## type 5
   agg1             <-   aggregate( M1  ~ type, data = All, mean )
   agg2             <-   aggregate( M2  ~ type, data = All, mean )
   aggd             <-   aggregate( Md  ~ type, data = All, mean )

   sdagg1           <-   aggregate( Msd1 ~ type, data = All, mean )
   sdagg2           <-   aggregate( Msd2 ~ type, data = All, mean )

   sagg1            <-   aggregate( S1  ~ type, data = All, mean )
   sagg2            <-   aggregate( S2  ~ type, data = All, mean )
   saggd            <-   aggregate( Sd  ~ type, data = All, mean )

   qagg1            <-   aggregate( abs(ti1)  ~ type, data = All, function(x) {quantile(x,0.95)} )
   qagg2            <-   aggregate( abs(ti2)  ~ type, data = All, function(x) {quantile(x,0.95)} )
   qaggd            <-   aggregate( abs(Mtdi) ~ type, data = All, function(x) {quantile(x,0.95)} )

   agg1             <-   merge(agg1,sagg1,by=c("type"))
   agg1             <-   merge(agg1,sdagg1,by=c("type"))
   agg1             <-   merge(agg1,qagg1,by=c("type"))
   agg2             <-   merge(agg2,sagg2,by=c("type"))
   agg2             <-   merge(agg2,sdagg2,by=c("type"))
   agg2             <-   merge(agg2,qagg2,by=c("type"))
   aggd             <-   merge(aggd,saggd,by=c("type"))
   aggd             <-   merge(aggd,qaggd,by=c("type"))

   tmp55            <-   merge(agg1,agg2,by=c("type"))
   tmp55            <-   merge(tmp55,aggd,by=c("type"))
   tmp55            <-   as.data.frame( as.list(tmp55))
   names(tmp55)     <-   c("type","M1","S1","sd1","q1","M2","S2","sd2","q2","Md","Sd","qd")

   tmp55$sdd        <-   sqrt( tmp55[,"sd1"]^2 + tmp55[,"sd2"]^2 )
   tab55            <-   tmp55
   tab55[,"lb1"]    <-   tmp55[,"S1"] - tmp55[,"q1"] * tmp55[,"sd1"]
   tab55[,"ub1"]    <-   tmp55[,"S1"] + tmp55[,"q1"] * tmp55[,"sd1"]
   tab55[,"lb2"]    <-   tmp55[,"S2"] - tmp55[,"q2"] * tmp55[,"sd2"]
   tab55[,"ub2"]    <-   tmp55[,"S2"] + tmp55[,"q2"] * tmp55[,"sd2"]
   tab55[,"lbd"]    <-   tmp55[,"Sd"] - tmp55[,"qd"] * tmp55[,"sdd"]
   tab55[,"ubd"]    <-   tmp55[,"Sd"] + tmp55[,"qd"] * tmp55[,"sdd"]


   tab55            <-   tab55[,c("type","M1","lb1","ub1","M2","lb2","ub2","Md","lbd","ubd")]

   stab <- tab55
}

   cnms             <-   paste0(c("lb","ub"),CItp)
   onms             <-   c("Est",cnms)
   selvars          <-   c("Est",cnms,"trt")

   ECI0             <-   M$ECI[ M$ECI[,"alpha1"] == 0 & M$ECI[,"alpha2"] == 0 & M$ECI[,"k"] == K, 
                                selvars ]

   Line4            <-   cbind( "MEX", ECI0[ ECI0[,"trt"] == 1, onms],
                                       ECI0[ ECI0[,"trt"] == 2, onms],
                                       ECI0[ ECI0[,"trt"] == 0, onms])

   names(stab)      <-   c("type","M1","lb1","ub1","M2","lb2","ub2","Md","lbd","ubd")
   names(Line4)     <-   c("type","M1","lb1","ub1","M2","lb2","ub2","Md","lbd","ubd")

   ETab3            <-   rbind(stab,Line4)

   SCI0             <-   M$SCI[ M$SCI[,"alpha1"] == 0 & M$SCI[,"alpha2"] == 0 & M$SCI[,"k"] == K, 
                                selvars ]

   Line4            <-   cbind( "MSX", SCI0[ SCI0[,"trt"] == 1, onms],
                                       SCI0[ SCI0[,"trt"] == 2, onms],
                                       SCI0[ SCI0[,"trt"] == 0, onms])
   names(Line4)     <-   c("type","M1","lb1","ub1","M2","lb2","ub2","Md","lbd","ubd")

   STab3 <- rbind(ETab3,Line4)
   rownames(STab3)  <- NULL

   return( STab3 )
}
