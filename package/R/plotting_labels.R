#' Short labelling function
#' @param x The text
#' @param width The width of the label
#' @export
label_short <- function(x, width = 10) {
  tibble(id = as.character(x)) %>%
    left_join(dynbenchmark::labels, "id") %>%
    mutate(short = ifelse(is.na(short), label_capitalise(id), short)) %>%
    mutate(short = label_wrap(short, width = width)) %>%
    pull(short)
}

#' Text wrapping
#'
#' @param x a character vector.
#' @param width a positive integer giving the target column for wrapping lines in the output.
#' @param collapse an optional character string to separate the different lines.
#'
#' @export
label_wrap <- function(x, width = 10, collapse = "\n") {
  strwrap(x, width, simplify = FALSE) %>% map_chr(paste0, collapse = collapse)
}

#' Long labelling function
#' @param x The text
#' @export
label_long <- function(x) {
  tibble(id = as.character(x)) %>%
    left_join(dynbenchmark::labels, "id") %>%
    mutate(long = ifelse(is.na(long), label_capitalise(id), long)) %>%
    pull(long)
}

#' Capitalise label
#' @param x The text
#' @export
label_capitalise <- function(x) {
  capitalise <- function(string) {
    capped <- grep("^[A-Z]", string, invert = TRUE)
    substr(string[capped], 1, 1) <- toupper(substr(string[capped], 1, 1))
    string
  }

  x %>% str_replace_all("_", " ") %>% capitalise()
}


#' Labeller for facets
#' @param label_func Which function to use for facet labelling
#' @export
label_facet <- function(label_func = label_long) {function(df) {mutate_all(df, label_func)}}

#' Label trajectory types simplified
#' @param x Trajectory types
#' @export
label_simple_trajectory_types <- function(x) {
  tibble(id = as.character(x)) %>%
    left_join(dynwrap::trajectory_types, by = "id") %>%
    mutate(label = label_long(ifelse(!is.na(simplified), simplified, x))) %>%
    .$label
}


#' Labels only the extrema (and zero)
#' @param x The values
#' @export
label_extrema <- function(x) {
  ifelse(x %in% c(0, min(x), max(x)), x, "")
}


#' Label p_value
#' @param p_values P values
#' @param cutoffs Cutoffs for number of asterisks
#'
#' @export
label_pvalue <- function(p_values, cutoffs = c(0.1, 0.01, 1e-5)) {
  requireNamespace("glue")

  breaks <- c(Inf, cutoffs, -Inf)
  labels <- rev(c("NS", map_chr(seq_len(length(cutoffs)), ~glue::glue_collapse(rep("*", .)))))
  p_values %>% cut(breaks, labels) %>% as.character()
}

