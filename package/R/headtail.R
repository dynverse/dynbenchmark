#' Retain the heads and tails of a character vector split by newlines
#'
#' @param X A character vector to process
#' @param num The number of lines to retain at the head and at the tail
#'
#' @importFrom pbapply pbsapply
#'
#' @export
headtail <- function(X, num) {
  pbapply::pbsapply(X, cl = 8, function(x) {
    xs <- strsplit(x, split = "\n")[[1]]
    if (length(xs) > 2 * num) {
      xs <- c(head(xs, num), paste0("... ", length(xs) - 2 * num, " lines omitted ..."), tail(xs, num))
    }
    paste0(c(xs, ""), collapse = "\n")
  })
}
