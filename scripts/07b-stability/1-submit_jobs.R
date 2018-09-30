library(dynbenchmark)
library(tidyverse)

experiment("07b-stability")

if (!file.exists(result_file("fitdata.rds"))) {
  benchmark_results_normalised <- read_rds(result_file("benchmark_results_normalised.rds", "07-benchmark"))
  benchmark_results_input <- read_rds(result_file("benchmark_results_input.rds", "07-benchmark"))
  benchmark_results_unnormalised <- read_rds(result_file("benchmark_results_unnormalised.rds", "07-benchmark"))

  dataset_ids <- benchmark_results_input$datasets_info$dataset_id
  metrics <- c("overall", benchmark_results_input$metrics)

  bench_compare <-
    benchmark_results_normalised$data_aggregations %>%
    filter(dataset_source == "mean") %>%
    select(method_id, param_id, dataset_trajectory_type, !!metrics) %>%
    gather(metric, experiment, !!metrics)

  benchmark_results_unnormalised$raw_data <-
    benchmark_results_unnormalised$raw_data %>%
    select(method_id, method_name, dataset_id, dataset_trajectory_type, dataset_source, param_id, prior_id, repeat_ix, error_status, time_method, !!benchmark_results_input$metrics) %>%
    mutate(time_method = ifelse(is.na(time_method), 3600, time_method))


  bools <- sample(c(1, 0), length(dataset_ids), replace = TRUE)

  fitness <- function(bools, benchmark_results_unnormalised, benchmark_results_input, dataset_ids, bench_compare, metrics) {
    # retain subset of datasets
    rawd <- benchmark_results_unnormalised$raw_data
    rawd <- rawd[rawd$dataset_id %in%  dataset_ids[bools == 1], , drop = FALSE]

    # replicate aggregation with subset of datasets
    out <- benchmark_aggregate(
      data = rawd,
      metrics = benchmark_results_input$metrics,
      norm_fun = benchmark_results_input$norm_fun,
      mean_fun = benchmark_results_input$mean_fun,
      mean_weights = benchmark_results_input$mean_weights,
      dataset_source_weights = benchmark_results_input$dataset_source_weights
    )

    # calculate percentage of original execution time
    pct_time <- sum(rawd$time_method) / sum(benchmark_results_unnormalised$raw_data$time_method)

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
    maxiter = 500,
    seed = 1,
    benchmark_results_unnormalised, benchmark_results_input, dataset_ids, bench_compare, metrics
  )

  did_sel <- dataset_ids[fit@solution[1,] == 1]

  readr::write_rds(lst(dataset_ids, did_sel, fit), result_file("fitdata.rds"))
}

list2env(readr::read_rds(result_file("fitdata.rds")), .GlobalEnv)

if (!file.exists(derived_file("design.rds"))) {
  timeout_sec <- 60 * 60
  memory_gb <- 16
  num_repeats <- 1
  num_bootstraps <- 10
  bootstrap_pct_cells <- .95
  bootstrap_pct_features <- .95

  metrics <- c("correlation", "edge_flip", "featureimp_cor", "featureimp_wcor", "F1_branches", "him")

  benchmark_results_input <- read_rds(result_file("benchmark_results_input.rds", "07-benchmark"))
  benchmark_results_unnormalised <- read_rds(result_file("benchmark_results_unnormalised.rds", "07-benchmark"))

  ##########################################################
  ###############       DEFINE METHODS       ###############
  ##########################################################

  method_ids <- benchmark_results_input$methods_info$method_id

  methods <-
    dynwrap::get_ti_methods(method_ids, evaluate = FALSE) %>%
    mapdf(function(m) {
      l <- m$fun()
      l$fun <- m$fun
      l$type <- "function"
      l
    }) %>%
    list_as_tibble() %>%
    select(id, type, fun, everything())

  default_parameters <- list(
    fateid = tibble(id = "default", force = TRUE),
    stemnet = tibble(id = "default", force = TRUE),
    tscan = tibble(id = "default", modelNames = list(c("VVV", "EEE")))
  )

  # combine default params and optimised params... if we had some!
  parameters <- lapply(method_ids, function(mn) {
    defaults <-
      if (mn %in% names(default_parameters)) {
        default_parameters[[mn]]
      } else {
        tibble(id = "default")
      }
    # best <- ... %>% mutate(id = "optimised")
    # bind_rows(default, best)
    defaults
  }) %>% set_names(method_ids)

  ##########################################################
  ###############       DEFINE DATASETS      ###############
  ##########################################################

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
      id = sprintf(paste0("scaling_%0", ceiling(log10(n())), "d"), seq_len(n())),
      type = "function",
      pct_cells = bootstrap_pct_cells,
      pct_features = bootstrap_pct_features,
      fun = pmap(
        lst(odid = orig_dataset_id, se = seed, pce = pct_cells, pfe = pct_features),
        function(odid, se, pce, pfe) {
          env <- new.env()
          lst(odid, se, pce, pfe, generate_dataset) %>% list2env(envir = env)
          fun <- function() {
            generate_dataset(orig_dataset_id = odid, pct_cells = pce, pct_features = pfe, seed = se)
          }
          environment(fun) <- env
          fun
        })
    ) %>%
    select(id, type, fun, everything())

  ##########################################################
  ###############       CREATE DESIGN        ###############
  ##########################################################
  design <-
    benchmark_generate_design(
      datasets = datasets,
      methods = methods,
      parameters = parameters,
      num_repeats = 1
    )

  predicted_times <-
    inner_join(
      benchmark_results_unnormalised$raw_data %>% select(orig_dataset_id = dataset_id, method_id, time_method),
      datasets %>% select(id, orig_dataset_id),
      by = "orig_dataset_id"
    ) %>%
    mutate(time_method = ifelse(is.na(time_method), timeout_sec, time_method))

  method_ord <-
    predicted_times %>%
    group_by(method_id) %>%
    summarise(time_method = sum(time_method)) %>%
    arrange(time_method) %>%
    pull(method_id)

  design$crossing <-
    design$crossing %>%
    mutate(
      method_order = match(method_id, method_ord)
    ) %>%
    arrange(method_order)

  # save configuration
  write_rds(design, derived_file("design.rds"), compress = "xz")
  write_rds(metrics, result_file("metrics.rds"), compress = "xz")
  write_rds(lst(timeout_sec, memory_gb, metrics, num_repeats, num_bootstraps, bootstrap_pct_cells, bootstrap_pct_features, orig_dataset_ids = did_sel), result_file("params.rds"), compress = "xz")
}

##########################################################
###############        SUBMIT JOB          ###############
##########################################################
design_filt <- read_rds(derived_file("design.rds"))
list2env(read_rds(result_file("params.rds")), environment())

# step 1:
# design_filt$crossing <- design_filt$crossing %>% filter(method_id %in% c("identity", "scorpius", "paga", "mst"))

# submit job
benchmark_submit(
  design = design_filt,
  qsub_grouping = "{method_id}/{param_id}",
  qsub_params = lst(timeout = timeout_sec, memory = paste0(memory_gb, "G")),
  metrics = metrics,
  verbose = TRUE,
  output_models = FALSE
)
