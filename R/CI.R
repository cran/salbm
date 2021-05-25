# ----------------------------------------------------------------------------
# given a list, lst,  of times and alpha combinations, this calculates CIs for
# each combination in the list.
# ----------------------------------------------------------------------------
CI <- function( lst, type, im1r, im1se, is1r, is1se, im2r, im2se, is2r, is2se ) {
  al1               <-    lst$alpha1
  al2               <-    lst$alpha2
  t                 <-    lst$t
  
  R1                <-    selectOne( t = t, mr = im1r, mse = im1se, sr = is1r, sse = is1se, type = type, alpha = al1 )
  R2                <-    selectOne( t = t, mr = im2r, mse = im2se, sr = is2r, sse = is2se, type = type, alpha = al2 )

  CIout             <-    mkOneCI( Est1 = R1$MEst, SEM1 = R1$MSE, SESub1 = R1$SEst, Est2 = R2$MEst, SEM2 = R2$MSE, SESub2 = R2$SEst )
  cnames            <-    colnames(CIout)
  CIout             <-    cbind( CIout, al1, al2, t )
  colnames(CIout)   <-    c( cnames, "alpha1", "alpha2", "k" )
  CIout             <-    as.data.frame(CIout)
  row.names(CIout)  <-    NULL
  return(CIout)
}
