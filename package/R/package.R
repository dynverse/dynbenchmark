#' Easy installing and loading of the dynverse
#'
#' The 'dynverse' is a set of packages with which to
#' evaluate trajectory inference methods for dynamic processes.
#'
#' The dynbenchmark package contains the code to generate/preprocess datasets, evaluate TI methods on these datasets, and finally wrap all results together in a scientific paper.
#' The code within this package can be run locally, but can also be run within a docker environment, for which the scripts are contained in the `scripts` folder.
#'
#' @import assertthat
#' @importFrom tibble is_tibble as_tibble as_data_frame tibble data_frame enframe deframe lst tribble rownames_to_column column_to_rownames
#' @import dplyr
#' @import ggplot2
#' @importFrom tidyr crossing
#' @importFrom purrr %>% %||% walk set_names map map_dbl map_lgl map_chr map_df map2 map2_dbl map2_lgl map2_chr map2_df invoke
#' @importFrom stringr str_replace_all str_replace str_detect str_sub str_subset str_glue
#'
#' @import dynutils
#' @import dynplot
#' @import dyneval
#' @import dynmethods
#' @import dynwrap
#' @import dynparam
#'
#' @docType package
#' @name dynbenchmark
#'
NULL

