library(dynbenchmark)
library(tidyverse)

experiment("07-benchmark")

##########################################################
############### PART TWO: RETRIEVE RESULTS ###############
##########################################################

# fetch results from cluster
benchmark_fetch_results(TRUE)

# bind results in one data frame (without models)
execution_output <- benchmark_bind_results(load_models = FALSE)

# df <- execution_output %>% filter(edge_flip < 0) %>% select(method_id, dataset_id, param_id, prior_id, repeat_ix)
# model <- load_dyneval_model(method_id = "celltrails/default", df = df, experiment_id = "07-benchmark")

design <- read_rds(derived_file("design.rds"))
methods_info <- design$methods %>%
  rename_all(function(x) paste0("method_", x)) %>%
  select(-method_type) %>%
  left_join(dynmethods::methods %>% select(method_id = id, method_type = type), by = "method_id")
datasets_info <- design$datasets %>% rename_all(function(x) paste0("dataset_", x))
# collect relevant trajectory types
trajtypes <-
  dynwrap::trajectory_types %>%
  filter(id %in% unique(datasets_info$dataset_trajectory_type)) %>%
  add_row(id = "overall", simplified = "overall", directed = TRUE, directedness = "directed", colour = "#AAAAAA", background_colour = "E6A1A1", ancestors = list(character(0)))

###################################################
############### CREATE AGGREGATIONS ###############
###################################################

# join <- inner_join(
#   design$crossing %>% select(-task_source, -settings, -model),
#   data %>% select(dataset_id, method_id, prior_id, repeat_ix, param_id, error_status, time = time_method, mem = max_mem),
#   by = c("dataset_id", "method_id", "prior_id", "repeat_ix", "param_id")
# ) %>%
#   filter(error_status %in% c("no_error", "time_limit", "memory_limit")) %>%
#   mutate(
#     time = ifelse(error_status == "memory_limit", NA, 10^time),
#     mem = ifelse(error_status == "time_limit" | mem < -100, NA, 10^mem),
#     ltime = log10(time),
#     lmem = log10(mem)
#   )


# scaling disabled for now
scalesigmoid_trafo <- function (x, remove_errored = TRUE, max_scale = TRUE) {
  x[x < 0] <- 0
  x[x > 1] <- 1
  xn <- x[!is.na(x)]
  x[is.na(x)] <- 0
  if (all(x == 0)) return(x)
  if (max_scale) {
    y <- (x - mean(xn)) / max(abs(xn - mean(xn))) * 5
  } else {
    y <- (x - mean(xn)) / var(xn) * 5
  }
  sigmoid::sigmoid(y)
}
#
# # previously:
# # trafo_fun <- percent_rank
# trafo_fun <- scalesigmoid_trafo
trafo_fun <- function(x) {
  ifelse(is.na(x), 0, x) %>% pmax(0) %>% pmin(1)
}

data <-
  execution_output %>%
  left_join(datasets_info %>% select(dataset_id, dataset_trajectory_type, dataset_source), by = "dataset_id") %>%
  left_join(methods_info %>% select(method_id, method_name), by = "method_id") %>%
  left_join(design$crossing %>% select(dataset_id, method_id, prior_id, repeat_ix, param_id, lpredtime, lpredmem, predtime, predmem), by = c("dataset_id", "method_id", "prior_id", "repeat_ix", "param_id")) %>%
  mutate(
    time = ifelse(error_status != "no_error", 6 * 3600, time_method),
    mem = ifelse(error_status != "no_error", 32 * 10e9, max_mem),
    ltime = log10(time),
    lmem = log10(mem),
    pct_errored = (error_status != "no_error") + 0,
    pct_time_limit = (error_status == "time_limit") + 0,
    pct_memory_limit = (error_status == "memory_limit") + 0,
    pct_execution_error = (error_status == "execution_error") + 0,
    pct_method_error = (error_status == "method_error") + 0,
    dataset_trajectory_type_f = factor(dataset_trajectory_type, levels = trajtypes$id)
  ) %>%
  group_by(dataset_id) %>%
  mutate(
    norm_correlation = trafo_fun(correlation),
    norm_edge_flip = trafo_fun(edge_flip),
    norm_featureimp_cor = trafo_fun(featureimp_cor),
    norm_F1_branches = trafo_fun(F1_branches),
    rank_time = percent_rank(-ltime),
    rank_mem = percent_rank(-lmem)
  ) %>%
  ungroup() %>%
  select(-stdout, -stderr, -error_message)

data %>% group_by(method_id, error_status) %>% summarise(n = n()) %>% as.data.frame()

# aggregate over replicates
data_repl <- data %>%
  group_by(method_id, method_name, dataset_id, param_id, dataset_trajectory_type, dataset_source, dataset_trajectory_type_f) %>%
  summarise_if(is.numeric, mean) %>%
  ungroup() %>%
  mutate(
    pct_method_error_all = (pct_method_error == 1)+0,
    pct_method_error_stoch = pct_method_error - pct_method_error_all,
    harm_mean = dyneval::calculate_harmonic_mean(norm_correlation, norm_edge_flip, norm_featureimp_cor, norm_F1_branches)
  )

# process trajtype grouped evaluation
data_trajtype <- data_repl %>%
  group_by(method_id, method_name, param_id, dataset_trajectory_type, dataset_source, dataset_trajectory_type_f) %>%
  mutate(n = n()) %>%
  summarise_if(is.numeric, mean) %>%
  ungroup() %>%
  mutate(
    harm_mean = dyneval::calculate_harmonic_mean(norm_correlation, norm_edge_flip, norm_featureimp_cor, norm_F1_branches)
  )

# process overall evaluation
data_method <- data_trajtype %>%
  group_by(method_id, method_name, param_id, dataset_source) %>%
  mutate(n = n()) %>%
  summarise_if(is.numeric, mean) %>%
  ungroup() %>%
  mutate(
    harm_mean = dyneval::calculate_harmonic_mean(norm_correlation, norm_edge_flip, norm_featureimp_cor, norm_F1_branches)
  )

# adding mean per trajtype
data_trajtype_totals <- bind_rows(
  data_trajtype,
  data_trajtype %>%
    group_by(method_id, method_name, param_id, dataset_trajectory_type, dataset_trajectory_type_f) %>%
    summarise_if(is.numeric, mean) %>%
    ungroup() %>%
    mutate(dataset_source = "mean")
) %>%
  mutate(
    harm_mean = dyneval::calculate_harmonic_mean(norm_correlation, norm_edge_flip, norm_featureimp_cor, norm_F1_branches)
  )

# adding mean per method
data_method_totals <-
  bind_rows(
    data_method,
    data_method %>%
      group_by(method_id, method_name, param_id) %>%
      summarise_if(is.numeric, mean) %>%
      ungroup() %>%
      mutate(dataset_source = "mean")
  ) %>%
  mutate(
    harm_mean = dyneval::calculate_harmonic_mean(norm_correlation, norm_edge_flip, norm_featureimp_cor, norm_F1_branches)
  )

# combine all aggregated data frames
data_trajtype_totalsx2 <- bind_rows(
  data_method_totals %>% mutate(dataset_trajectory_type = "overall"),
  data_trajtype_totals
) %>%
  mutate(dataset_trajectory_type_f = factor(dataset_trajectory_type, levels = trajtypes$id)) %>%
  mutate(
    harm_mean = dyneval::calculate_harmonic_mean(norm_correlation, norm_edge_flip, norm_featureimp_cor, norm_F1_branches)
  )

# save data structures
to_save <- environment() %>% as.list()
to_save <- to_save[c(str_subset(names(to_save), "^data"), "trajtypes", "datasets_info", "methods_info")]
write_rds(to_save, result_file("benchmark_results.rds"), compress = "xz")
