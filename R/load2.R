#' Load file and set its variable's name in one go
#'
#' @param file Path to the input file
#' @export
load2 <- function(file) {
  load(file)
  get(ls()[ls() != "file"])
}
