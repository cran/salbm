salbmGetCI <- function( obj, div ) {
  M       <-   obj
  Narm      <-   M$Narm
  Ro        <-   addSE(R=M, div=div)

  Lw        <-   c("R","RL","wts")
  nms1      <-   c( paste0( "Main", 1, Lw ), paste0("Main",1,"SE"),paste0("Samp", 1, Lw), paste0("Sub",1,"SE") )
  othNames  <-   c("K","alphas","Narm","ntree")

  out     <-   list()
  for ( i in 2:Narm ) {
    nmsi       <-   c( paste0( "Main",i, Lw ), paste0("Main",i,"SE"),paste0("Samp", i, Lw), paste0("Sub",i,"SE") )
    Ri         <-   Ro[ c( nms1, nmsi, othNames ) ]
    Resi       <-   addCIM(R=Ri,carm=i)
    nmo        <-   paste0("Res1",i)
    out[[nmo]] <-   Resi
  }
  return(out)
}
