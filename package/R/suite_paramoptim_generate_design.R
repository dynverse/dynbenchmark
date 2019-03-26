#' Generate a benchmarking design
#'
#' @param datasets The datasets to be used in the evaluation.
#'   Must be a named list consisting of dataset ids (character),
#'   dynwrap::data_wrapper's, or functions that generate dynwrap::data_wrapper's.
#' @param methods The methods to be evaluated.
#'   Must be a named list consisting of method_ids (character) or dynwrap::ti_wrapper's.
#' @param priors A list of lists. Each sublist contains a list of priors that each method is allowed to optionally use.
#'   Check \code{\link[dynwrap:priors]{dynwrap::priors}} for a list of possible priors. Default priors given is "none"
#' @param num_repeats The number of times to repeat the evaluation.
#' @param crossing A data frame containing the combinations of methods, datasets and priors to be evaluated.
#'   Must be a data frame containing the columns dataset_id, method_id, prior_id, repeat_ix and param_id
#'
#' @examples
#' \dontrun{
#' library(tibble)
#'
#' datasets <- list(
#'   "synthetic/dyntoy/bifurcating_1",
#'   dyntoy::generate_dataset(id = "test1"),
#'   test2 = function() dyntoy::generate_dataset()
#' )
#'
#' methods <- list(
#'   "comp1",
#'   dynmethods::ti_scorpius(),
#'   "tscan"
#' )
#'
#' priors <- list(
#'   list(id = "none", method_id = c("comp1", "scorpius", "tscan")),
#'   list(id = "some", method_id = c("tscan"), set = c("start_id", "end_n"))
#' )
#'
#' paramoptim_generate_design(
#'   datasets = datasets,
#'   methods = methods,
#'   priors = priors,
#'   num_repeats = 2
#' )
#' }
#' @export
paramoptim_generate_design <- function(
  datasets,
  methods,
  priors = NULL,
  num_repeats = 1,
  crossing = NULL
) {
  check_paramoptim_design_parameters(
    datasets = datasets,
    methods = methods,
    priors = priors,
    num_repeats = num_repeats
  )

  datasets <- process_datasets_design(datasets)
  methods <- process_methods_design(methods)
  priors <- process_priors_design(priors, methods$id)

  if (is.null(crossing)) {
    # generate designs of the different parts of the evaluation
    crossing <- crossing(
      dataset_id = factor(datasets$id),
      method_id = factor(methods$id),
      repeat_ix = seq_len(num_repeats)
    ) %>%
      left_join(priors %>% select(prior_id = id, method_id), by = "method_id")
  } else {
    testthat::expect_true(all(c("method_id", "dataset_id", "repeat_ix", "prior_id") %in% colnames(crossing)))
  }

  # all combinations of the different parts
  list(
    datasets = datasets,
    methods = methods,
    priors = priors,
    crossing = crossing
  )
}


check_paramoptim_design_parameters <- function(
  datasets,
  methods,
  priors,
  num_repeats
) {
  if (!check_design_datasets(datasets, generate_error = FALSE) && !is.character(datasets)) {
    check <- is.list(datasets) && all(sapply(datasets, function(x) is.character(x) || is.function(x) || dynwrap::is_wrapper_with_expression(x)))
    if (!check) {
      stop("datasets is supposed be a vector of dataset ids, a list of dynwrap datasets, or a list of functions which will generate a dynwrap dataset")
    }
  }

  if (check_design_methods(methods, generate_error = FALSE)) {
    method_names <- methods$id
  } else if (!is.character(methods)) {
    check <- is.list(methods) && all(sapply(methods, function(x) is.character(x) || dynwrap::is_ti_method(x)))
    if (!check) {
      stop("methods is supposed be a vector of methods ids, or a list of dynwrap ti methods")
    }
    method_names <- sapply(methods, function(x) if (is.character(x)) x else x$method$id)
  } else {
    method_names <- methods
  }

  assert_that(is.numeric(num_repeats))
}
