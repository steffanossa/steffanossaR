#' Extract SS loadings, Proportion Var and Cumulative Var from factanal output
#'
#' @param factanal_object Output from *factanal()* call
#' @export
ex_factanal <- function(factanal_object) {
  if (class(factanal_object) != "factanal") {
    stop("need result of a 'factanal'")
  }
  loadings <- factanal_object$loadings
  b <- colSums(loadings^2)
  
  rbind(`SS loadings` = b,
        `Proportion Var` = b/nrow(loadings),
        `Cumulative Var` = cumsum(b/nrow(loadings)))
}
