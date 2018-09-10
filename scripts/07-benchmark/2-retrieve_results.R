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

# # hotfix for trajectory types
# fix_trajectory_types <- function(trajectory_types) {
#   unname(c(
#     "directed_linear" = "linear",
#     "directed_cycle" = "cycle",
#     "undirected_linear" = "linear",
#     "undirected_cycle" = "cycle",
#     "simple_fork" = "bifurcation",
#     "bifurcation" = "bifurcation",
#     "convergence" = "convergence",
#     "complex_fork" = "multifurcation",
#     "multifurcation" = "multifurcation",
#     "rooted_tree" = "tree",
#     "unrooted_tree" = "tree",
#     "directed_acyclic_graph" = "acyclic_graph",
#     "rooted_binary_tree" = "tree",
#     "unrooted_binary_tree" = "tree",
#     "directed_graph" = "graph",
#     "undirected_graph" = "graph",
#     "disconnected_directed_graph" = "disconnected_graph",
#     "disconnected_undirected_graph" = "disconnected_graph"
#   )[trajectory_types])
# }

# df <- execution_output %>% filter(edge_flip < 0) %>% select(method_id, dataset_id, param_id, prior_id, repeat_ix)
# model <- load_dyneval_model(method_id = "celltrails/default", df = df, experiment_id = "07-benchmark")

design <- read_rds(derived_file("design.rds"))

methods_info <- design$methods %>%
  rename_all(function(x) paste0("method_", x)) %>%
  select(-method_type) %>%
  left_join(dynmethods::methods %>% select(method_id = id, method_type = type), by = "method_id")
datasets_info <- design$datasets %>%
  # mutate(trajectory_type = fix_trajectory_types(trajectory_type)) %>%
  rename_all(function(x) paste0("dataset_", x))

crossing <- design$crossing

# collect relevant trajectory types
trajtypes <-
  dynwrap::trajectory_types %>%
  filter(id %in% unique(datasets_info$dataset_trajectory_type)) %>%
  add_row(id = "overall", colour = "#AAAAAA", background_colour = "E6A1A1", ancestors = list(character(0))) %>%
  mutate(id = factor(id, levels = id))


metrics <- read_rds(result_file("metrics.rds"))

# save data
write_rds(lst(trajtypes, metrics, datasets_info, methods_info), result_file("benchmark_results_input.rds"), compress = "xz")

#########################################
############### JOIN DATA ###############
#########################################

raw_data <-
  execution_output %>%
  select(-stdout, -stderr, -error_message) %>%
  left_join(methods_info %>% select(method_id, method_name), by = "method_id") %>%
  left_join(datasets_info %>% select(dataset_id, dataset_trajectory_type, dataset_source), by = "dataset_id") %>%
  left_join(crossing %>% select(dataset_id, method_id, prior_id, repeat_ix, param_id, lpredtime, lpredmem, predtime, predmem), by = c("dataset_id", "method_id", "prior_id", "repeat_ix", "param_id")) %>%
  mutate(dataset_trajectory_type = factor(dataset_trajectory_type, levels = levels(trajtypes$id))) %>%
  left_join(trajtypes %>% select(dataset_trajectory_type = id), by = "dataset_trajectory_type") %>%
  mutate(
    time = ifelse(error_status != "no_error", 6 * 3600, time_method),
    mem = ifelse(error_status != "no_error", 32 * 10e9, max_mem),
    ltime = log10(time),
    lmem = log10(mem)
  )

# save data
write_rds(lst(trajtypes, metrics, datasets_info, methods_info), result_file("benchmark_results_input.rds"), compress = "xz")
write_rds(lst(raw_data, metrics), result_file("benchmark_results_unnormalised.rds"), compress = "xz")


###################################################
############### CREATE AGGREGATIONS ###############
###################################################
out <- benchmark_aggregate(
  data = raw_data,
  metrics = metrics,
  norm_fun = "scalesigmoid",
  mean_fun = "geometric",
  mean_weights = c("correlation" = 1, "edge_flip" = 1, "featureimp_wcor" = 1, "F1_branches" = 1),
  dataset_source_weights = c("real" = 5, "synthetic/dyngen" = 5, "synthetic/dyntoy" = 1, "synthetic/prosstt" = 1, "synthetic/splatter" = 1)
)


# save data
write_rds(out, result_file("benchmark_results_normalised.rds"), compress = "xz")


