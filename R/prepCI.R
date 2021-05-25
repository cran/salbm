# --------------------------------------------------------
# mr is a main dataset estimate
# mse is a main dataset standard error estimate
# srse contains three columns, sub the bootstrap number,
# the result from each bootstrap, and the standard error
# estimate for each bootstrap.
#
# prepCI produces various values based on these:
# the bootstrap estimate minus the main estimate,
# t values etc.
# These in turn are used to create CIs.
# --------------------------------------------------------
prepCI <- function( mr, mse, srse ) {
  ## mr and mse are numbers, srse has sub, sampResult and sampSE in that order
  forCI             <-     srse  
  colnames( forCI ) <-     c("sub","sampR","sampSE")
  forCI[,"mainR"]   <-     mr
  forCI[,"mainSE"]  <-     mse  
  forCI[, "d"]      <-     forCI[, "sampR" ] - forCI[, "mainR" ]
  forCI[, "t"]      <-     (forCI[, "sampR" ] - forCI[, "mainR" ]) / forCI[, "sampSE" ]
  forCI[, "meanSR"] <-     mean( forCI[,"sampR"] )  
  forCI[,"cor"]     <-     2*forCI[,"mainR"] - forCI[,"meanSR"]
  forCI[, "cort"]   <-     (forCI[, "sampR" ] - forCI[, "cor" ]) / forCI[, "sampSE" ]
  return(forCI)
}
