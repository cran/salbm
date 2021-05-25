colDot  <-  function(x,q,col=c( "#EEEEFF99", "#EEEEFFAA", "#EEEEFFBB", "#EEEEFFCC", "#EEEEFFFF",  "#22223399", "#222233AA", "#222233BB", "#222233CC", "#222233FF")) {

  ocol    <-  rep("#44447799",length(x))
##  ocol[ x < q[5] ] <- col[1]
  ocol[ x < q[4] ] <- col[2]
  ocol[ x < q[3] ] <- col[3]
  ocol[ x < q[2] ] <- col[4]
  ocol[ x < q[1] ] <- col[5]
##  ocol[ x > q[5] ] <- col[6]
  ocol[ x > q[6] ] <- col[7]
  ocol[ x > q[7] ] <- col[8]
  ocol[ x > q[8] ] <- col[9]
  ocol[ x > q[9] ] <- col[10]
  return(ocol)
}

