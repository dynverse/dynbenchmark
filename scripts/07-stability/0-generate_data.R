#' Generate the subsampled datasets
#' First choose a set of datasets to be used
#' Then subsample each dataset

library(dynbenchmark)
library(tidyverse)

experiment("07-stability")

num_bootstraps <- 10
bootstrap_pct_cells <- .95
bootstrap_pct_features <- .95


##########################################################
###############      DEFINE DATASETS       ###############
##########################################################

if (!file.exists(result_file("fitdata.rds"))) {
  benchmark_results_normalised <- read_rds(result_file("benchmark_results_normalised.rds", "06-benchmark"))
  benchmark_results_input <- read_rds(result_file("benchmark_results_input.rds", "06-benchmark"))
  benchmark_results_unnormalised <- read_rds(result_file("benchmark_results_unnormalised.rds", "06-benchmark"))

  dataset_ids <- benchmark_results_input$datasets_info$dataset_id
  metrics <- c("overall", benchmark_results_input$metrics)

  bench_compare <-
    benchmark_results_normalised$data_aggregations %>%
    filter(dataset_source == "mean") %>%
    select(method_id, param_id, dataset_trajectory_type, !!metrics) %>%
    gather(metric, experiment, !!metrics)

  benchmark_results_unnormalised <-
    benchmark_results_unnormalised %>%
    select(method_id, method_name, dataset_id, dataset_trajectory_type, dataset_source, param_id, prior_id, repeat_ix, error_status, time_method, !!benchmark_results_input$metrics) %>%
    mutate(time_method = ifelse(is.na(time_method), 3600, time_method))


  bools <- sample(c(1, 0), length(dataset_ids), replace = TRUE)

  fitness <- function(bools, benchmark_results_unnormalised, benchmark_results_input, dataset_ids, bench_compare, metrics) {
    # retain subset of datasets
    rawd <- benchmark_results_unnormalised
    rawd <- rawd[rawd$dataset_id %in%  dataset_ids[bools == 1], , drop = FALSE]

    # replicate aggregation with subset of datasets
    out <- benchmark_aggregate(
      data = rawd
    )

    # calculate percentage of original execution time
    pct_time <- sum(rawd$time_method) / sum(benchmark_results_unnormalised$time_method)

    # join previous scores with current scores
    # why can't i use dplyr Y_Y
    subset_compare <- out$data_aggregations
    subset_compare <- subset_compare[subset_compare$dataset_source == "mean", , drop = FALSE]
    subset_compare <- subset_compare[, c("method_id", "param_id", "dataset_trajectory_type", metrics), drop = FALSE]
    subset_compare <- gather(subset_compare, "metric", "subset", !!metrics)

    joined <- full_join(bench_compare, subset_compare, by = setdiff(colnames(bench_compare), "experiment"))
    joined$subset[is.na(joined$subset)] <- 0

    # calculate scores
    corscore <- nacor(joined$experiment, joined$subset)
    boolscore <- 1 - mean(bools) / 20 # 0.95: we were able to throw away all datasets! 1: we had to retain all datasets
    timescore <- 1 - pct_time / 20 # 0.95: pipeline ran for 0 seconds! 1: nothing changed time-wise
    dyneval::calculate_geometric_mean(corscore, boolscore, timescore)
  }

  fit <- GA::ga(
    type = "binary",
    fitness = fitness,
    nBits = length(dataset_ids),
    parallel = TRUE,
    popSize = 8 * 8,
    maxiter = 2000,
    seed = 1,
    maxFitness = .99,
    benchmark_results_unnormalised, benchmark_results_input, dataset_ids, bench_compare, metrics
  )

  did_sel <- dataset_ids[fit@solution[1,] == 1]

  readr::write_rds(lst(dataset_ids, did_sel, fit), result_file("fitdata.rds"))
}

list2env(readr::read_rds(result_file("fitdata.rds")), .GlobalEnv)


# use helper function to generate datasets
source(scripts_file("generate_dataset.R"))

set.seed(1)


# construct datasets tibble
datasets <-
  crossing(
    orig_dataset_id = did_sel,
    seed = sample.int(10000, num_bootstraps)
  ) %>%
  as_tibble() %>%
  mutate(
    id = sprintf(paste0("stability_%0", ceiling(log10(n())), "d"), seq_len(n())),
    type = "function",
    pct_cells = bootstrap_pct_cells,
    pct_features = bootstrap_pct_features,
    fun = map(id, ~ function() readr::read_rds(dynbenchmark::derived_file(c(., ".rds"), experiment_id = "07-stability/dataset")))
  ) %>%
  select(id, type, fun, everything())

# create datasets and save at the remote's derived file folder
num_cores <- 1
handle <- qsub::qsub_lapply(
  X = seq_len(nrow(datasets)), # submit in reverse order so the first tasks will take the longest
  qsub_packages = c("tidyverse", "dynbenchmark", "dynwrap"),
  qsub_environment = c("datasets", "num_cores", "generate_dataset"),
  qsub_config = qsub::override_qsub_config(name = "datastability", memory = "10G", num_cores = num_cores, wait = FALSE, max_wall_time = "12:00:00"),
  FUN = function(i) {
    filename <- dynbenchmark::derived_file(c(datasets$id[[i]], ".rds"), experiment_id = "07-stability/dataset")
    # check whether dataset already exists
    if (file.exists(filename)) {
      success <-
        tryCatch({
          cat("Reading previous data file\n", sep = "")
          dataset <- readr::read_rds(filename)
          TRUE
        }, error = function(e) {
          FALSE
        })
      if (success) {
        cat("File already generated!\n", sep = "")
        return(TRUE)
      } else {
        cat("Could not read previous data file; starting again\n", sep = "")
        file.remove(filename)
      }
    }
    params <- as.list(datasets[i, ])
    dataset <- generate_dataset(
      orig_dataset_id = params$orig_dataset_id,
      pct_cells = params$pct_cells,
      pct_features = params$pct_features,
      seed = params$seed,
      cores = num_cores
    )

    readr::write_rds(dataset, filename, compress = "xz")

    TRUE
  }
)

readr::write_rds(datasets, path = derived_file("datasets.rds"), compress = "xz")
readr::write_rds(lst(num_bootstraps, bootstrap_pct_cells, bootstrap_pct_features), path = derived_file("dataset_params.rds"), compress = "xz")

#' @examples
#' download datasets from prism
#' qsub::rsync_remote(
#'   remote_src = TRUE,
#'   path_src = derived_file(experiment_id = "07-stability/dataset", remote = TRUE),
#'   remote_dest = FALSE,
#'   path_dest = derived_file(experiment_id = "07-stability/dataset", remote = FALSE),
#'   verbose = TRUE,
#'   compress = FALSE
#' )
