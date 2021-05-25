# ----------------------------------------------------------
# Takes a salbm results object R and adds standard errors
# to the object.
# The object R should contain weights for the main data and
# for each bootstrap.
# div is a positive number used to divide the alphas. It may
# be thought of as the alpha where E[Y_K| alpha] is maximum
#
# Uses:  mkalphawts and wtse
# ----------------------------------------------------------
addSE <- function(R, div)
{
   K          <-   R$K
   alphas     <-   R$alphas
   Narm       <-   R$Narm

   wts        <-   mkalphawts( alphas = alphas, div = div )

   for ( i in 1:Narm ) {
     Mnm   <- sprintf("Main%dwts",i)
     Snm   <- sprintf("Samp%dwts",i)
     Mnmo  <- sprintf("Main%dSE",i)
     Snmo  <- sprintf("Sub%dSE",i)

     Mwts  <- R[[ Mnm ]]
     Swts  <- R[[ Snm ]]

     MSE   <- wtse( W = Mwts, wts = wts, NT = K, alphas = alphas )
     SSE   <- wtse( W = Swts, wts = wts, NT = K, alphas = alphas, sub = TRUE ) 

     R[[ Mnmo ]] <-  MSE
     R[[ Snmo ]] <-  SSE
   }
   return(R)
}
