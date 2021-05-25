# ----------------------------------------------------------
# E0, E1 and Em where E0 estimates E[Y_t] when 0 is
# substituted for missing values, E1 estimate E[Y_t] when 1
# is substituted for missing values and Em estimates E[Y_t]
# when the mean value at a timepoint is substituted for
# missing values.  S0, S1 and Sm are defined similarly. 
# 
# This function computes standard errors based on weights
# applied to standard deviations from bootstrap samples.
# ----------------------------------------------------------
wtse <- function( W, wts, NT, alphas, sq = 0, sub = FALSE ) {
  E0 <- W[ W[,"type"] == "E0", ]
  E1 <- W[ W[,"type"] == "E1", ]
  Em <- W[ W[,"type"] == "Em", ]

  S0 <- W[ W[,"type"] == "S0", ]
  S1 <- W[ W[,"type"] == "S1", ]
  Sm <- W[ W[,"type"] == "Sm", ]

  if ( sub ) {
    for ( t in 1:NT ) {
       zsub <- Em[ Em[,t] == 0, "sub" ]
       E0[ E0[,"sub"] %in% zsub, t] <- 0 
       E1[ E1[,"sub"] %in% zsub, t] <- 0 
       zsub <- Sm[ Sm[,t] == 0, "sub" ]
       S0[ S0[,"sub"] %in% zsub, t] <- 0 
       S1[ S1[,"sub"] %in% zsub, t] <- 0 
    }
  }
  for ( i in 1:length(alphas) ) {
    alpha  <-  alphas[i]
    if ( sq == 0 ) {
      twtese  <-  wts[i,2] * E0[,1:NT] + wts[i,3] * Em[,1:NT] + wts[i,4] * E1[,1:NT]
    } else {
      twtese  <-  sqrt(wts[i,2] * E0[,1:NT]^2 + wts[i,3] * Em[,1:NT]^2 + wts[i,4] * E1[,1:NT]^2)
    }
    twtese[,"alpha"]  <-  alpha
    twtese[,"type" ]  <-  "E" 
    if ( sub ) twtese[,"sub"  ] <- Em[,"sub"]  
    
    if ( sq == 0 ) {
      twtsse  <-  wts[i,2] * S0[,1:NT] + wts[i,3] * Sm[,1:NT] + wts[i,4] * S1[,1:NT]
    } else {
      twtsse  <-  sqrt( wts[i,2] * S0[,1:NT]^2 + wts[i,3] * Sm[,1:NT]^2 + wts[i,4] * S1[,1:NT]^2)
    }
    twtsse[,"alpha"]  <-  alpha
    twtsse[,"type" ]  <-  "S" 
    if ( sub ) twtsse[,"sub"  ] <- Sm[,"sub" ]  

    twtse             <-  rbind( twtese, twtsse )
    if ( i == 1 ) {
      WtSE            <-  twtse 
    } else {
      WtSE            <-  rbind(WtSE,twtse)
    }
  }
  rownames(WtSE) <- NULL
  return(WtSE)
}
