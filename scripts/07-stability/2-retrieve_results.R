library(dynbenchmark)
library(tidyverse)

experiment("07-stability")

##########################################################
############### PART TWO: RETRIEVE RESULTS ###############
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
execution_output <- benchmark_bind_results(load_models = FALSE) %>%
  filter(!method_id %in% c("identity", "shuffle", "error"))
table(execution_output$method_id, execution_output$error_status)

#########################################
############### JOIN DATA ###############
#########################################

datasets <- read_rds(derived_file("datasets.rds", "07-stability"))

raw_data <-
  execution_output %>%
  rename(did_bs = dataset_id) %>%
  left_join(datasets %>% select(did_bs = id, dataset_id = orig_dataset_id), by = "did_bs") %>%
  left_join(load_datasets() %>% select(dataset_id = id, dataset_trajectory_type = trajectory_type, dataset_source = source), by = "dataset_id")

write_rds(raw_data, result_file("benchmark_results_unnormalised.rds"), compress = "xz")


###################################################
############### CREATE AGGREGATIONS ###############
###################################################
benchmark_results_input <- read_rds(result_file("benchmark_results_input.rds", "06-benchmark"))
stability_params <- read_rds(result_file("params.rds", "07-stability"))

out <- benchmark_aggregate(
  data = raw_data %>% mutate(method_name = method_id),
  metrics = stability_params$metrics,
  norm_fun = benchmark_results_input$norm_fun,
  mean_fun = benchmark_results_input$mean_fun,
  mean_weights = benchmark_results_input$mean_weights,
  dataset_source_weights = benchmark_results_input$dataset_source_weights
)
out$data$overall <- ifelse(is.finite(out$data$overall), out$data$overall, 0)

#####################################################
############### CALCULATE VARIABILITY ###############
#####################################################

worst_var <- var(c(rep(0, floor(stability_params$num_bootstraps / 2)), rep(1, ceiling(stability_params$num_bootstraps / 2))))

norm_var <- function(x) {
  maxx <- max(x)

  if (maxx == 0) return(1)

  worst_var_this <- worst_var * maxx ^ 2

  (worst_var_this - var(x) ) / worst_var_this
}

met2 <- c(stability_params$metrics, "overall")

data_stab <-
  out$data %>%
  select(method_id, dataset_id, param_id, !!met2) %>%
  group_by(method_id, dataset_id, param_id) %>%
  summarise_if(is.numeric, norm_var) %>%
  ungroup() %>%
  group_by(method_id, param_id) %>%
  summarise_if(is.numeric, mean) %>%
  ungroup() %>%
  gather(metric, value, !!met2) %>%
  mutate(metric = paste0("stability_metric_", metric))

data_stab_overall <-
  data_stab %>%
  group_by(method_id, param_id) %>%
  summarise(value = dyneval::calculate_arithmetic_mean(value)) %>%
  mutate(metric = "stability_overall_overall")

data_stab <- bind_rows(data_stab, data_stab_overall)


#####################################################
############### SAVE DATA ###############
#####################################################

write_rds(data_stab, result_file("stability_results.rds"), compress = "xz")

