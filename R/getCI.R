getCI <- function( Res, ci=c("lb4","ub4") ) {
  ECI     <-  Res$ECI
  SCI     <-  Res$SCI
  K       <-  Res$K
  alphas  <-  Res$alphas

  kpnms   <-  c("alpha1","Est",ci)
  ER1     <-  ECI[ ECI[,"k"] == K & ECI[,"trt"] == 1 & ECI[,"alpha1"] == ECI[,"alpha2"], kpnms]
  ER2     <-  ECI[ ECI[,"k"] == K & ECI[,"trt"] == 2 & ECI[,"alpha1"] == ECI[,"alpha2"], kpnms]
  ERD     <-  ECI[ ECI[,"k"] == K & ECI[,"trt"] == 0 & ECI[,"alpha1"] == ECI[,"alpha2"], kpnms]

  SR1     <-  SCI[ SCI[,"k"] == K & SCI[,"trt"] == 1 & SCI[,"alpha1"] == SCI[,"alpha2"], kpnms]
  SR2     <-  SCI[ SCI[,"k"] == K & SCI[,"trt"] == 2 & SCI[,"alpha1"] == SCI[,"alpha2"], kpnms]
  SRD     <-  SCI[ SCI[,"k"] == K & SCI[,"trt"] == 0 & SCI[,"alpha1"] == SCI[,"alpha2"], kpnms]

  return( list( K=K, alphas=alphas, ER1 = ER1, ER2 = ER2, ERD = ERD, SR1 = SR1, SR2 = SR2, SRD = SRD ) )
}
