#' Register cores for parallel processing on Unix & Windows
#' @note just a wrap for unix and windows systems. DoParallel should theoretically work on both, though. Will leave 1 core for os if not run on r-workbench.
#' @export
#' 
#' @importFrom doParallel registerDoParallel
#' @importFrom doMC registerDoMC
#' @importFrom parallel detectCores makePSOCKcluster
#' @importFrom crayon green
registerPar <- function() {
  os_type <- Sys.info()["sysname"]
  if (os_type == "Windows") {
    n_cores <- parallel::detectCores(logical = F) - 1
    doParallel::registerDoParallel(makePSOCKcluster(n_cores))
  } else {
    if (Sys.info()["nodename"] == "r-workbench") {
      n_cores <- parallel::detectCores()
      doMC::registerDoMC(n_cores)
    } else {
      # see above
      n_cores <- parallel::detectCores() - 1
      doMC::registerDoMC(n_cores - 1)
    }
  }
  cat(crayon::green(n_cores, "cores registered for parallel processing on", os_type))
}