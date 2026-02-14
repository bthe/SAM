##' Simulate data from fitted model and re-estimate from each run  
##' @param fit a fitted model returned from sam.fit 
##' @param nsim number of simulations 
##' @param ncores number of cores to be used
##' @importFrom parallel detectCores makeCluster parLapply stopCluster clusterEvalQ
##' @export
simstudy <- function(fit, nsim, ncores = detectCores()){
  if (length(ncores) == 0 || is.na(ncores[1]) || !is.finite(ncores[1]) || ncores[1] < 1) {
    ncores <- 1L
  } else {
    ncores <- as.integer(ncores[1])
  }
  simdata <- simulate(fit, nsim=nsim,  full.data=TRUE)
  if(ncores>1){
    cl <- makeCluster(ncores) #set up nodes
    on.exit(stopCluster(cl)) #shut it down
    lib.ver <- dirname(path.package("stockassessment"))
    clusterExport(cl, varlist="lib.ver", envir=environment())
    clusterEvalQ(cl, {library(stockassessment, lib.loc=lib.ver)})
    runs <- parLapply(cl, simdata, function(x)sam.fit(x, fit$conf, fit$obj$env$par))
  } else {
    runs <- lapply(simdata, function(x)sam.fit(x, fit$conf, fit$obj$env$par))   
  }
  attr(runs, "fit") <- fit
  class(runs)<-"samset"
  runs
}
