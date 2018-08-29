#' Renders equations for github markdown
#'
#' @param x A character vector
#' @param format The format: html, markdown, latex, ...
#' @export
render_equations <- function(x, format = get_default_format()) {
  if (format == "markdown") {
    x <- str_replace_all(
      x,
      "\\$[^\\$]+\\$",
      function(x) paste0("![](https://latex.codecogs.com/gif.latex?", URLencode(str_sub(x, 2, -2)), ")")
    )
  }

  x
}
