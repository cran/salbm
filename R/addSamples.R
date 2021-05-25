addSamples <- function(obj, NBootstraps = 0, bBS = 1, 
                       nseeds = c(5,9), nseeds2 = c(-4,-5), 
                       returnJP = TRUE, returnSamples = FALSE,... ) {
  UseMethod("addSamples")
}
