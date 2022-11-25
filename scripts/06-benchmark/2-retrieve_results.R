#' Retrieve the results from the gridengine cluster and aggregate the data

library(dynbenchmark)
library(tidyverse)

experiment("06-benchmark")

##########################################################
###            RETRIEVE RESULTS FROM PRISM             ###
##########################################################

# If you are the one who submitted the jobs, run:
benchmark_fetch_results(TRUE)
# qsub::rsync_remote(
#   remote_src = FALSE,
#   path_src = derived_file(remote = FALSE, experiment = "06-benchmark"),
#   remote_dest = TRUE,
#   path_dest = derived_file(remote = TRUE, experiment = "06-benchmark"),
#   verbose = TRUE,
#   exclude = "*/r2gridengine/*"
# )

# If you want to download the output from prism
# qsub::rsync_remote(
#   remote_src = TRUE,
#   path_src = derived_file(remote = TRUE, experiment = "06-benchmark"),
#   remote_dest = FALSE,
#   path_dest = derived_file(remote = FALSE, experiment = "06-benchmark"),
#   verbose = TRUE,
#   exclude = "*/r2gridengine/*"
# )

# bind results in one data frame (without models)
execution_output <- benchmark_bind_results(load_models = FALSE)

execution_output %>% filter(error_status == "execution_error") %>% pull(error_message) %>% first() %>% cat

design <- read_rds(derived_file("design.rds"))

methods_info <- design$methods %>%
  select(-type, -fun) %>%
  rename(method_id = id)
datasets_info <- design$datasets %>%
  rename_all(function(x) paste0("dataset_", x))

crossing <- design$crossing

# collect relevant trajectory types
trajtypes <-
  dynwrap::trajectory_types %>%
  filter(id %in% unique(datasets_info$dataset_trajectory_type)) %>%
  add_row(id = "overall", colour = "#AAAAAA", background_colour = "#E6A1A1", ancestors = list(character(0))) %>%
  mutate(id = factor(id, levels = id))

list2env(read_rds(result_file("params.rds")), environment())

# save data
write_rds(lst(trajtypes, metrics, datasets_info, methods_info), result_file("benchmark_results_input.rds"), compress = "xz")

##########################################################
###                      JOIN DATA                     ###
##########################################################
execution_output$stdout <- headtail(execution_output$stdout, 50)
# execution_output <- execution_output %>% select(-stdout)

raw_data <-
  execution_output %>%
  left_join(methods_info %>% select(method_id, method_name), by = "method_id") %>%
  left_join(datasets_info %>% select(dataset_id, dataset_trajectory_type, dataset_source), by = "dataset_id") %>%
  left_join(crossing %>% select(dataset_id, method_id, prior_id, repeat_ix, param_id, time_lpred, mem_lpred, time_pred, mem_pred), by = c("dataset_id", "method_id", "prior_id", "repeat_ix", "param_id")) %>%
  mutate(dataset_trajectory_type = factor(dataset_trajectory_type, levels = levels(trajtypes$id))) %>%
  left_join(trajtypes %>% select(dataset_trajectory_type = id), by = "dataset_trajectory_type") %>%
  mutate(
    time = ifelse(error_status != "no_error", 6 * 3600, time_method),
    mem = ifelse(error_status != "no_error", 32 * 10e9, max_mem),
    ltime = log10(time),
    lmem = log10(mem),
    featureimp_wcor = ifelse(!is.finite(featureimp_wcor), 0, featureimp_wcor)
  )


table(raw_data$dataset_source, raw_data$error_status)
write_rds(raw_data, result_file("benchmark_results_unnormalised.rds"), compress = "xz")

##########################################################
###              DETERMINE SOURCE WEIGHTS              ###
##########################################################

raw_data <-
  read_rds(result_file("benchmark_results_unnormalised.rds")) %>%
  filter(!method_id %in% c("identity", "error", "shuffle", "random"))

sources <- unique(raw_data$dataset_source)

tmp <- benchmark_aggregate(
  data = raw_data %>% filter(error_status == "no_error"),
  dataset_source_weights = set_names(rep(1, length(sources)), sources)
)

dataset_source_weights <-
  tmp$data_aggregations %>%
  filter(dataset_trajectory_type == "overall") %>%
  select(method_id:dataset_source, overall) %>%
  spread(dataset_source, overall) %>%
  select(-mean) %>%
  mutate(compare = `real/gold`) %>%
  gather(synth, value, -method_id:-dataset_trajectory_type, -compare) %>%
  group_by(synth) %>%
  summarise(cor = nacor(compare, value)) %>%
  deframe()

# do not remove this line! this rds needs to be written as it gets used by `dynbenchmark:::get_default_dataset_source_weights()` ... :rolling_eyes:
write_rds(dataset_source_weights, result_file("dataset_source_weights.rds"))

##########################################################
###                 AGGREGATE RESULTS                  ###
##########################################################
raw_data <- read_rds(result_file("benchmark_results_unnormalised.rds", "06-benchmark"))

out <- benchmark_aggregate(
  data = raw_data
)

##########################################################
###                      SAVE DATA                     ###
##########################################################
write_rds(lst(trajtypes, metrics, datasets_info, methods_info, dataset_source_weights), result_file("benchmark_results_input.rds"), compress = "xz")
write_rds(out, result_file("benchmark_results_normalised.rds"), compress = "xz")

