#' pmap, but with qsub ^_^
#'
#' @inheritParams purrr::pmap
#' @param ... Params given to [qsub::qsub_lapply()]
#' @seealso qsub::qsub_lapply
#' @export
qsub_pmap <- function(.x, .f, ...) {
  number_of_elements <- unique(map_int(.x, length))
  testthat::expect_equal(length(number_of_elements), 1)
  .x2 <- map(seq_len(number_of_elements), function(ix) {
    .x2_element <- map(.x, ~.[[ix]]) %>% set_names(names(.x))
    .x2_element$.f <- .f
    .x2_element
  })

  .f2 <- function(.x, ...) {purrr::invoke(.x$.f, .x[names(.x) != ".f"], ...)}

  qsub_lapply(
    .x2,
    .f2,
    ...
  )
}
