getXCI <- function( Res, ci = c("lb4","ub4") ) {
  ECI     <-  Res$ECI
  SCI     <-  Res$SCI
  K       <-  Res$K
  alphas  <-  Res$alphas

  kpnms   <-  c("alpha1","alpha2","Est",ci)
  ER1     <-  ECI[ ECI[,"k"] == K & ECI[,"trt"] == 1, kpnms]
  ER2     <-  ECI[ ECI[,"k"] == K & ECI[,"trt"] == 2, kpnms]
  ERD     <-  ECI[ ECI[,"k"] == K & ECI[,"trt"] == 0, kpnms]

  SR1     <-  SCI[ SCI[,"k"] == K & SCI[,"trt"] == 1, kpnms]
  SR2     <-  SCI[ SCI[,"k"] == K & SCI[,"trt"] == 2, kpnms]
  SRD     <-  SCI[ SCI[,"k"] == K & SCI[,"trt"] == 0, kpnms]

  return( list( K=K, alphas=alphas, ER1 = ER1, ER2 = ER2, ERD = ERD, SR1 = SR1, SR2 = SR2, SRD = SRD ) )
}
