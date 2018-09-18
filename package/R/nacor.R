#' Na removed cor
#' @inheritParams stats::cor
#' @importFrom stats cor
#' @export
nacor <- function(x, y) {
  is_na <- is.na(x) | is.na(y)
  stats::cor(x[!is_na], y[!is_na])
}
