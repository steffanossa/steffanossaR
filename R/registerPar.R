#' Register cores for parallel processing on Unix & Windows
#' @note just a wrap for unix and windows systems. DoParallel should theoretically work on both, though. Will leave 1 core for os if not run on r-workbench.
#' @export
registerPar <- function() {
  os_type <- Sys.info()["sysname"]
  if (os_type == "Windows") {
    n_cores <- parallel::detectCores(logical = F) - 1
    DoParallel::registerDoParallel(makePSOCKcluster(n_cores))
  } else {
    if (Sys.info()["nodename"] == "r-workbench") {
      n_cores <- parallel::detectCores()
      DoMC::registerDoMC(n_cores)
    } else {
      # see above
      n_cores <- parallel::detectCores() - 1
      DoMC::registerDoMC(n_cores - 1)
    }
  }
  cat(crayon::green(n_cores, "cores registered for parallel processing on", os_type))
}