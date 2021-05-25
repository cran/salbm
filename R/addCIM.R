addCIM <- function(R,carm=2)
{  
  K                 <-  R$K
  alphas            <-  R$alphas

  im1r              <-  R$Main1RL
  im1se             <-  R$Main1SE
  is1r              <-  R$Samp1RL
  is1se             <-  R$Sub1SE

  nmvec             <-  c(paste0("Main",carm,"RL"),paste0("Main",carm,"SE"),paste0("Samp",carm,"RL"),paste0("Sub",carm,"SE")) 
  im2r              <-  R[[ nmvec[1] ]] ##  Main2RL
  im2se             <-  R[[ nmvec[2] ]] ##  Main2SE
  is2r              <-  R[[ nmvec[3] ]] ##  Samp2RL
  is2se             <-  R[[ nmvec[4] ]] ##  Sub2SE

  times             <-  1:K
  alpha1            <-  alphas
  alpha2            <-  alphas
  WantM             <-  expand.grid(alpha1,alpha2,times)
  colnames(WantM)   <-  c("alpha1","alpha2","t") 
  WantList          <-  apply(WantM,1,as.list)

  ECI  <-  lapply( WantList, CI, type=1, im1r = im1r, im1se = im1se, is1r = is1r, is1se = is1se,
                                         im2r = im2r, im2se = im2se, is2r = is2r, is2se = is2se )
  ECI  <-  do.call(rbind,ECI)

  SCI  <-  lapply( WantList, CI, type=2, im1r = im1r, im1se = im1se, is1r = is1r, is1se = is1se,
                                         im2r = im2r, im2se = im2se, is2r = is2r, is2se = is2se )
  SCI  <-  do.call(rbind,SCI)

  R$ECI  <- ECI
  R$SCI  <- SCI
  return(R)
}
