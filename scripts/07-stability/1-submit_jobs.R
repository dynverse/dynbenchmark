#' Submit the stability jobs

library(dynbenchmark)
library(tidyverse)

experiment("07-stability")

if (!file.exists(derived_file("design.rds"))) {
  timeout_sec <- 60 * 60
  memory_gb <- 16
  num_repeats <- 1

  metrics <- c("correlation", "featureimp_wcor", "F1_branches", "him")

  list2env(readr::read_rds(path = derived_file("dataset_params.rds")), environment())
  benchmark_results_input <- read_rds(result_file("benchmark_results_input.rds", "06-benchmark"))
  benchmark_results_unnormalised <- read_rds(result_file("benchmark_results_unnormalised.rds", "06-benchmark"))

  ##########################################################
  ###############       DEFINE METHODS       ###############
  ##########################################################

  method_ids <- benchmark_results_input$methods_info$method_id

  methods <-
    dynwrap::get_ti_methods(method_ids, evaluate = FALSE) %>%
    mapdf(function(m) {
      l <- m$fun()
      k <- list()
      for (n in names(l)) {
        for (p in names(l[[n]])) {
          k[[paste0(n, "_", p)]] <- l[[n]][[p]]
        }
      }
      k$fun <- m$fun
      k$type <- "function"
      k
    }) %>%
    list_as_tibble() %>%
    select(id = method_id, type, fun, everything())

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
  datasets <- readr::read_rds(derived_file("datasets.rds"))

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
      benchmark_results_unnormalised %>% select(orig_dataset_id = dataset_id, method_id, time_method),
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

  orig_dataset_ids <- unique(datasets$orig_dataset_id)

  # save configuration
  write_rds(design, derived_file("design.rds"), compress = "xz")
  write_rds(lst(timeout_sec, memory_gb, metrics, num_repeats, num_bootstraps, bootstrap_pct_cells, bootstrap_pct_features, orig_dataset_ids), result_file("params.rds"), compress = "xz")
}

##########################################################
###############        SUBMIT JOB          ###############
##########################################################
design_filt <- read_rds(derived_file("design.rds"))
list2env(read_rds(result_file("params.rds")), environment())

# step 1:
design_filt$crossing <- design_filt$crossing %>% filter(method_id %in% c("scorpius", "monocle_ica", "monocle_ddrtree", "celltree_gibbs", "celltree_vem", "celltree_maptpx", "sincell", "celltrails", "gpfates"))

# submit job
benchmark_submit(
  design = design_filt,
  qsub_grouping = "{method_id}/{param_id}",
  qsub_params = lst(timeout = timeout_sec, memory = paste0(memory_gb, "G")),
  metrics = metrics,
  verbose = TRUE,
  output_models = TRUE
)
