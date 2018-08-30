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
  add_row(id = "overall", simplified = "overall", directed = TRUE, directedness = "directed", colour = "#AAAAAA", background_colour = "E6A1A1", ancestors = list(character(0))) %>%
  mutate(
    id = factor(id, levels = id),
    simplified = factor(simplified, levels = unique(simplified))
  )


###################################################
############### AGGREGATION SETTINGS ##############
###################################################

scalesigmoid_trafo <- function (x, remove_errored = TRUE, max_scale = FALSE) {
  x[x < 0] <- 0
  x[x > 1] <- 1
  xn <- x[!is.na(x)]
  x[is.na(x)] <- 0
  if (length(x) == 1 || all(x == 0)) return(x)
  if (max_scale) {
    y <- (x - mean(xn)) / max(abs(xn - mean(xn))) * 5
  } else {
    y <- (x - mean(xn)) / var(xn) * 5
  }
  sigmoid::sigmoid(y)
}

checkrange_fun <- function(x) {
  ifelse(is.na(x), 0, x) %>% pmax(0) %>% pmin(1)
}

metrics <- read_rds(result_file("metrics.rds"))

calc_mean <- function(df) {
  df %>% mutate(
    overall_harm_norm = dyneval::calculate_harmonic_mean(norm_correlation, norm_edge_flip, norm_featureimp_wcor, norm_F1_branches),
    overall_arit_norm = dyneval::calculate_arithmetic_mean(norm_correlation, norm_edge_flip, norm_featureimp_wcor, norm_F1_branches),
    overall_geom_norm = dyneval::calculate_geometric_mean(norm_correlation, norm_edge_flip, norm_featureimp_wcor, norm_F1_branches),
    overall_harm_unno = dyneval::calculate_harmonic_mean(correlation, edge_flip, featureimp_wcor, F1_branches),
    overall_arit_unno = dyneval::calculate_arithmetic_mean(correlation, edge_flip, featureimp_wcor, F1_branches),
    overall_geom_unno = dyneval::calculate_geometric_mean(correlation, edge_flip, featureimp_wcor, F1_branches),
    overall = overall_geom_norm
  )
}

dataset_source_weights <- c("real" = 1, "synthetic/dyngen" = 1, "synthetic/dyntoy" = 1/3, "synthetic/prosstt" = 1/3, "synthetic/splatter" = 1/3)


###################################################
############### CREATE AGGREGATIONS ###############
###################################################

data <-
  execution_output %>%
  left_join(datasets_info %>% select(dataset_id, dataset_trajectory_type, dataset_source), by = "dataset_id") %>%
  left_join(methods_info %>% select(method_id, method_name), by = "method_id") %>%
  left_join(design$crossing %>% select(dataset_id, method_id, prior_id, repeat_ix, param_id, lpredtime, lpredmem, predtime, predmem), by = c("dataset_id", "method_id", "prior_id", "repeat_ix", "param_id")) %>%
  mutate(dataset_trajectory_type = factor(dataset_trajectory_type, levels = levels(trajtypes$id))) %>%
  left_join(trajtypes %>% select(dataset_trajectory_type = id, dataset_trajectory_type_simplified = simplified), by = "dataset_trajectory_type") %>%
  mutate(
    time = ifelse(error_status != "no_error", 6 * 3600, time_method),
    mem = ifelse(error_status != "no_error", 32 * 10e9, max_mem),
    ltime = log10(time),
    lmem = log10(mem),
    pct_errored = (error_status != "no_error") + 0,
    pct_time_limit = (error_status == "time_limit") + 0,
    pct_memory_limit = (error_status == "memory_limit") + 0,
    pct_execution_error = (error_status == "execution_error") + 0,
    pct_method_error = (error_status == "method_error") + 0
  ) %>%
  group_by(dataset_id) %>%
  mutate_at(metrics, checkrange_fun) %>%
  mutate_at(set_names(metrics, paste0("norm_", metrics)), scalesigmoid_trafo) %>%
  mutate(
    rank_time = percent_rank(-ltime),
    rank_mem = percent_rank(-lmem)
  ) %>%
  ungroup() %>%
  select(-stdout, -stderr, -error_message) %>%
  calc_mean()

data %>% group_by(method_id, error_status) %>% summarise(n = n()) %>% as.data.frame()

data %>% group_by(method_id) %>% summarise(n = n()) %>% as.data.frame() %>% arrange(n)


# aggregate over replicates
data_repl <- data %>%
  group_by(method_id, method_name, dataset_id, param_id, dataset_trajectory_type_simplified, dataset_source) %>%
  select(-repeat_ix) %>%
  summarise_if(is.numeric, mean) %>%
  ungroup() %>%
  mutate(
    pct_method_error_all = (pct_method_error == 1)+0,
    pct_method_error_stoch = pct_method_error - pct_method_error_all
  ) %>%
  calc_mean()

# process trajtype grouped evaluation
data_trajtype <- data_repl %>%
  group_by(method_id, method_name, param_id, dataset_trajectory_type_simplified, dataset_source) %>%
  mutate(n = n()) %>%
  summarise_if(is.numeric, mean) %>%
  ungroup() %>%
  calc_mean()

# process overall evaluation
data_method <- data_trajtype %>%
  group_by(method_id, method_name, param_id, dataset_source) %>%
  mutate(n = n()) %>%
  summarise_if(is.numeric, mean) %>%
  ungroup() %>%
  calc_mean() %>%
  mutate(
    dataset_trajectory_type_simplified = factor("overall", levels = levels(trajtypes$simplified))
  )

data_aggregations <-
  bind_rows(data_trajtype, data_method) %>% {
  df <- .
  bind_rows(
    df,
    df %>%
      gather(metric, value, -method_id:-dataset_source) %>%
      group_by(method_id, method_name, param_id, dataset_trajectory_type_simplified, metric) %>%
      mutate(dataset_weight = dataset_source_weights[dataset_source]) %>%
      summarise(value = sum(value * dataset_weight) / sum(dataset_weight)) %>%
      ungroup() %>%
      spread(metric, value) %>%
      mutate(dataset_source = "mean")
  )
}


# save data structures
write_rds(lst(data, data_aggregations, trajtypes, metrics, datasets_info, methods_info), result_file("benchmark_results.rds"), compress = "xz")

## CHECK VARIANCES PER DATASET AND METRIC
stat_funs <- c("var", "mean")
metricso <- c("overall", metrics)

dat_df <-
  data %>%
  select(method_id, dataset_id, !!metricso) %>%
  gather(metric, score, !!metricso) %>%
  group_by(dataset_id, metric) %>%
  filter(n() > 2) %>%
  rename(unnorm = score) %>%
  mutate(norm = scalesigmoid_trafo(unnorm)) %>%
  gather(type, score, unnorm, norm) %>%
  mutate(type = factor(type, levels = c("unnorm", "norm"))) %>%
  ungroup()

var_df <-
  dat_df %>%
  group_by(type, dataset_id, metric) %>%
  summarise_at(vars(score), stat_funs) %>%
  ungroup()

g <- ggplot(var_df) +
  geom_point(aes(mean, var, colour = metric)) +
  facet_wrap(~type) +
  scale_colour_brewer(palette = "Dark2") +
  theme_bw()

# g
ggsave(result_file("normalisation_var_mean.pdf"), g, width = 10, height = 5)

