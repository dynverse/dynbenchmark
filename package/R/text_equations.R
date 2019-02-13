#' Renders equations for github markdown
#'
#' @param x A character vector
#' @param format The format: html, markdown, latex, ...
#' @export
render_equations <- function(x, format = get_default_format()) {
  if (format == "markdown") {
    # inline equation
    x <- str_replace_all(
      x,
      "\\$[^\\$]+\\$",
      function(x) paste0("![](https://latex.codecogs.com/gif.latex?", URLencode(str_sub(x, 2, -2)), ")")
    )

    # multiple line equations, e.g.
    # $$
    # 1 + 2 = 3
    # $$

    which_dd <- which(x == "$$")
    if (length(which_dd) %% 2 == 1) {
      stop("Uneven number of '$$' lines found!")
    }

    for (i in seq_len(length(which_dd) / 2)) {
      start <- which_dd[i * 2 - 1] + 1
      end <- which_dd[i * 2] - 1

      url <- paste0("![](https://latex.codecogs.com/gif.latex?", URLencode(paste0(x[start:end], collapse = "\n")), ")")

      x[start] <- url
      x[start - 1] <- ""
      x[(start):end+1] <- ""
    }
  }

  x
}
