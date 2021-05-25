# --------------------------------------------------------------
# dfac.vec takes a vector x; if x is a factor it returns a
# de-factored version of x, and otherwise it returns x. 
# --------------------------------------------------------------
dfac.vec <- function(x) {
   if ( is.factor(x) ) { 
     return(as.numeric( levels(x) )[x])
   } else {
     return(x)
   }
}

# --------------------------------------------------------------
# dfac applies dfac.vec to each column in a dataframe.
# --------------------------------------------------------------
dfac <- function(D) {
   D[] <- lapply(D,dfac.vec)
   return(D)
}

# --------------------------------------------------------------
# dfac.list applies dfac.vec to each dataframe in a list,
# --------------------------------------------------------------
dfac.list <- function(xl) {
   xl <- lapply(xl,function(x) { x[] <- lapply( x, dfac.vec ); return(x) } )
   return(xl)
}
