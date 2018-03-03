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
