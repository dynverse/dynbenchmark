#' Generate a benchmarking design
#'
#' @param datasets The datasets to be used in the evaluation.
#'   Must be a named list consisting of dataset ids (character),
#'   dynwrap::data_wrapper's, or functions that generate dynwrap::data_wrapper's.
#' @param methods The methods to be evaluated.
#'   Must be a named list consisting of method_ids (character) or dynwrap::ti_wrapper's.
#' @param parameters A named list containing data frames of the parameters to evaluate.
#'   The names of the list must be present in method_ids.
#'   The data frames must be of format `data_frame(id = "set1", param1 = "a", param2 = 2.0)`.
#' @param priors A list of lists. Each sublist contains a list of priors that each method is allowed to optionally use.
#'   Check \code{\link[dynwrap:priors]{dynwrap::priors}} for a list of possible priors. Default priors given is "none"
#' @param num_repeats The number of times to repeat the evaluation.
#' @param crossing A data frame containing the combinations of methods, datasets, parameters and priors to be evaluated.
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
#' parameters <- list(
#'   scorpius = tibble(
#'     id = c("test1", "default"),
#'     params = list(
#'       list(ndim = 5),
#'       list()
#'     )
#'   ),
#'   tscan = tibble(
#'     id = paste0("test", 1:3),
#'     clusternum_lower = 4:6,
#'     clusternum_upper = 18:20
#'   )
#' )
#'
#' priors <- list(
#'   list(id = "none"),
#'   list(id = "some", set = c("start_id", "end_n"))
#' )
#'
#' benchmark_generate_design(
#'   datasets = datasets,
#'   methods = methods,
#'   parameters = parameters,
#'   priors = priors,
#'   num_repeats = 2
#' )
#' }
#' @export
benchmark_generate_design <- function(
  datasets,
  methods,
  parameters = NULL,
  priors = NULL,
  num_repeats = 1,
  crossing = NULL
) {
  check_benchmark_design_parameters(
    datasets,
    methods,
    parameters,
    priors,
    num_repeats
  )

  datasets <- process_datasets_design(datasets)
  methods <- process_methods_design(methods)
  parameters <- process_parameters_design(methods, parameters)
  priors <- process_priors_design(priors)

  if (is.null(crossing)) {
    # generate designs of the different parts of the evaluation
    crossing <- crossing(
      dataset_id = factor(datasets$id),
      method_id = factor(methods$id),
      prior_id = factor(priors$id),
      repeat_ix = seq_len(num_repeats)
    ) %>%
      left_join(parameters %>% select(param_id = id, method_id), by = "method_id")
  } else {
    testthat::expect_true(all(c("method_id", "dataset_id", "repeat_ix", "prior_id", "param_id") %in% colnames(crossing)))
  }

  # all combinations of the different parts
  list(
    datasets = datasets,
    methods = methods,
    priors = priors,
    parameters = parameters,
    crossing = crossing
  )
}

check_benchmark_design_parameters <- function(
  datasets,
  methods,
  parameters,
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

  assert_that(parameters$method_id %all_in% method_names)

  assert_that(is.numeric(num_repeats))
}

process_datasets_design <- function(datasets) {
  if (check_design_datasets(datasets, generate_error = FALSE)) {
    return(datasets)
  }
  out <- map_df(seq_along(datasets), function(di) {
    d <- datasets[[di]]

    dataset_out <-
      if (is.character(d)) {
        tibble(
          id = d,
          type = "character",
          fun = list(function() load_dataset(d, as_tibble = FALSE))
        )
      } else if (dynwrap::is_wrapper_with_expression(d)) {
        tibble(
          id = d$id,
          type = "dynwrap",
          fun = list(function() d)
        )
      } else if (is.function(d)) {
        id <- names(datasets)[[di]]
        testthat::expect_false(is.null(id), info = "dataset functions need to be named")
        tibble(
          id = id,
          type = "function",
          fun = list(d)
        )
      } else {
        stop("datasets must be a named list consisting of dataset ids (character), dynwrap::data_wrapper's, or functions that generate dynwrap::data_wrapper's")
      }

    if (dataset_out$type[[1]] %in% c("dynwrap", "character")) {
      env <- new.env()
      assign("d", d, envir = env)
      environment(dataset_out$fun[[1]]) <- env
    }

    dataset_out
  })

  testthat::expect_false(any(duplicated(out$id)))

  out
}

process_methods_design <- function(methods) {
  if (check_design_methods(methods, generate_error = FALSE)) {
    return(methods)
  }
  out <- map_df(methods, function(m) {
    if (is.character(m)) {
      get_ti_methods(method_ids = m) %>%
        mutate(type = "character") %>%
        select(id, type, fun)
    } else if (dynwrap::is_ti_method(m)) {
      fun <- function() m
      env <- new.env()
      assign("m", m, envir = env)
      environment(fun) <- env
      tibble(
        id = m$method$id,
        type = "ti_method",
        fun = list(fun)
      )
    } else {
      stop("methods must be a named list consisting of method_ids (character) or dynwrap::ti_wrapper's")
    }
  })

  testthat::expect_false(any(duplicated(out$id)))

  out
}

process_parameters_design <- function(methods, parameters) {
  for (mn in methods$id) {
    if (!mn %in% names(parameters)) {
      parameters[[mn]] <- tibble(id = "default")
    }
  }

  map_df(
    methods$id,
    function(mn) {
      prm <- parameters[[mn]] %>% as_tibble()
      if ("paramset_id" %in% colnames(prm)) {
        prm <- prm %>% select(id = paramset_id)
      }
      if ("params" %in% colnames(prm)) {
        prm %>%
          mutate(method_id = mn) %>%
          select(id, method_id, params)
      } else {
        params <- mapdf(prm, ~ .[names(.) != "id"])
        tibble(id = prm$id, method_id = mn, params = params)
      }
    }
  ) %>% mutate(
    method_id = factor(method_id, levels = methods$id)
  )
}

process_priors_design <- function(priors) {
  priors <- priors %||% list(list(id = "none", set = c()))
  map_df(
    priors,
    function(pr) {
      tibble(
        id = pr$id,
        set = list(pr$set)
      )
    }
  )
}
