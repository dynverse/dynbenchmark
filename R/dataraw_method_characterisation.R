#' Topology inference
#' @export
topinf <- tibble(
  name = c("free", "parameter", "fixed"),
  short_name = c("free", "param", "fixed"),
  colour = c("#00ab1b", "#edb600", "#cc2400")
)

#' Topology inference colours
#' @export
topinf_colours <- setNames(topinf$colour, topinf$name)

#' Errors
#' @export
#' @importFrom RColorBrewer brewer.pal
errors <- tibble(
  name = c("pct_memory_exceeded", "pct_time_exceeded", "pct_allerrored", "pct_stochastic"),
  label = c("Memory limit exceeded", "Time limit exceeded", "Dataset-specific error", "Stochastic error"),
  colour = RColorBrewer::brewer.pal(4, "Set3")
)
