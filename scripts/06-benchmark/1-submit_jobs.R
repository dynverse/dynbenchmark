#' Submit the jobs to the gridengine cluster

library(dynbenchmark)
library(tidyverse)

experiment("06-benchmark")

options(dynwrap_backend = "r_wrapper")

if (!file.exists(derived_file("design.rds"))) {
  timeout_sec <- 60 * 60
  memory_gb <- 16
  num_repeats <- 1
  metrics <- c("correlation", "featureimp_wcor", "F1_branches", "him")

  ##########################################################
  ###############       DEFINE METHODS       ###############
  ##########################################################

  scaling <- read_rds(result_file("scaling.rds", "05-scaling"))

  # need to look into scaling results of these methods first
  method_ids <- scaling$models$method_id

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
  datasets <-
    load_datasets() %>%
    mutate(
      lnrow = log10(map_dbl(cell_ids, length)),
      lncol = log10(map_dbl(feature_info, nrow))
    ) %>%
    select_if(is.atomic) %>%
    mutate(
      type = "character",
      fun = map(id, ~ function() load_dataset(., as_tibble = FALSE))
    )

  # determine method execution order by predicting
  # the running times of each method
  predicted_times <-
    pmap_df(scaling$models, function(method_id, predict_time, predict_mem, ...) {
      datasets2 <- datasets %>% rename(dataset_id = id)
      datasets2$method_id <- method_id
      datasets2$time_lpred = pmin(log10(predict_time(10^datasets2$lnrow, 10^datasets2$lncol)), log10(timeout_sec))
      datasets2$mem_lpred = pmin(log10(predict_mem(10^datasets2$lnrow, 10^datasets2$lncol)), log10(memory_gb * 1e9))
      datasets2
    }) %>%
    mutate(time_pred = 10^time_lpred, mem_pred = 10^mem_lpred)

  preds_dataset <-
    predicted_times %>%
    group_by(dataset_id) %>%
    summarise_at(c("time_lpred", "mem_lpred", "time_pred", "mem_pred"), sum) %>%
    mutate(
      category = paste0("Cat", cut(log10(time_pred), breaks = 5, labels = FALSE))
    )

  datasets <- datasets %>% left_join(preds_dataset %>% select(id = dataset_id, category), by = "id")

  preds_dataset %>% group_by(category) %>% summarise(time_pred = sum(time_pred), n = n()) %>% mutate(realtime = time_pred / 3600 / 192)

  ##########################################################
  ###############       CREATE DESIGN        ###############
  ##########################################################
  design <-
    benchmark_generate_design(
      datasets = datasets,
      methods = methods,
      parameters = parameters,
      num_repeats = num_repeats
    )

  method_ord <-
    predicted_times %>%
    group_by(method_id) %>%
    summarise(time_lpred = mean(time_lpred), mem_lpred = mean(mem_lpred)) %>%
    arrange(time_lpred) %>%
    pull(method_id)

  design$crossing <- design$crossing %>%
    left_join(preds_dataset %>% select(dataset_id, category), by = "dataset_id") %>%
    left_join(predicted_times, by = c("method_id", "dataset_id")) %>%
    mutate(
      method_order = match(method_id, method_ord)
    ) %>%
    arrange(category, method_order)

  # save configuration
  write_rds(design, derived_file("design.rds"), compress = "xz")
  write_rds(metrics, result_file("metrics.rds"), compress = "xz")
  write_rds(lst(timeout_sec, memory_gb, metrics, num_repeats), result_file("params.rds"), compress = "xz")
}

##########################################################
###############        SUBMIT JOB          ###############
##########################################################
design_filt <- read_rds(derived_file("design.rds"))
list2env(read_rds(result_file("params.rds")), environment())

design_filt$crossing <- design_filt$crossing %>% filter(method_id == "scorpius")

# step 1:
# design_filt$crossing <- design_filt$crossing %>% filter(method_id %in% c("identity", "scorpius", "paga", "mst"), category == "Cat1")

# step 2:
# design_filt$crossing <- design_filt$crossing %>% filter(category %in% c("Cat1", "Cat2"))

# step 3:
# design_filt$crossing <- design_filt$crossing %>% filter(category %in% c("Cat1", "Cat2", "Cat3"))

# step 4:

# submit job
benchmark_submit(
  design = design_filt,
  qsub_grouping = "{method_id}/{param_id}/{category}",
  qsub_params = lst(timeout = timeout_sec, memory = paste0(memory_gb, "G")),
  metrics = metrics,
  verbose = TRUE,
  output_models = TRUE
)
