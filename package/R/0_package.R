#' Easy installing and loading of the dynverse
#'
#' The 'dynverse' is a set of packages with which to
#' evaluate trajectory inference methods for dynamic processes.
#'
#' The dynbenchmark package contains the code to generate/preprocess datasets, evaluate TI methods on these datasets, and finally wrap all results together in a scientific paper.
#' The code within this package can be run locally, but can also be run within a docker environment, for which the scripts are contained in the `scripts` folder.
#'
#' @import tibble
#' @import dplyr
#' @importFrom purrr %>% map set_names map_dbl map2_dbl
#' @importFrom stringr str_replace_all str_replace
#'
#' @docType package
#' @name dynbenchmark
#'
NULL

