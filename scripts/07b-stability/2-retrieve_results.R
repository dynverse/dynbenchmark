library(dynbenchmark)
library(tidyverse)

experiment("07b-stability")

##########################################################
############### PART TWO: RETRIEVE RESULTS ###############
##########################################################

# If you are the one who submitted the jobs, run:
benchmark_fetch_results(TRUE)
# qsub::rsync_remote(
#   remote_src = FALSE,
#   path_src = derived_file(remote = FALSE, experiment = "07-benchmark"),
#   remote_dest = TRUE,
#   path_dest = derived_file(remote = TRUE, experiment = "07-benchmark"),
#   verbose = TRUE,
#   exclude = "*/r2gridengine/*"
# )

# If you want to download the output from prism
# qsub::rsync_remote(
#   remote_src = TRUE,
#   path_src = derived_file(remote = TRUE, experiment = "07-benchmark"),
#   remote_dest = FALSE,
#   path_dest = derived_file(remote = FALSE, experiment = "07-benchmark"),
#   verbose = TRUE,
#   exclude = "*/r2gridengine/*"
# )

# bind results in one data frame (without models)
execution_output <- benchmark_bind_results(load_models = FALSE)# %>%
  # filter(!method_id %in% c("identity", "shuffle", "error"))
table(execution_output$method_id, execution_output$error_status)

#########################################
############### JOIN DATA ###############
#########################################

raw_data <-
  execution_output %>%
  left_join(load_datasets() %>% select(dataset_id = id, dataset_trajectory_type = trajectory_type, dataset_source = source), by = "dataset_id")

# table(raw_data$dataset_source, raw_data$error_status)
# ###########################################
# ############### NORM PARAMS ###############
# ###########################################
#
#
# norm_fun <- "normal"
# mean_fun <- "geometric"
# mean_weights <- c("correlation" = 1, "him" = 1, "featureimp_wcor" = 1, "F1_branches" = 1)
#
# tmp <- benchmark_aggregate(
#   data = raw_data %>% filter(error_status == "no_error"),
#   metrics = metrics,
#   norm_fun = norm_fun,
#   mean_fun = mean_fun,
#   mean_weights = mean_weights,
#   dataset_source_weights = c("real/gold" = 1, "real/silver" = 1, "synthetic/dyngen" = 1, "synthetic/dyntoy" = 1, "synthetic/prosstt" = 1, "synthetic/splatter" = 1)
# )
#
# dataset_source_weights <-
#   tmp$data_aggregations %>%
#   filter(dataset_trajectory_type == "overall") %>%
#   select(method_id:dataset_source, overall) %>%
#   spread(dataset_source, overall) %>%
#   select(-mean) %>%
#   mutate(compare = `real/gold`) %>%
#   gather(synth, value, -method_id:-dataset_trajectory_type, -compare) %>%
#   group_by(synth) %>%
#   summarise(cor = nacor(compare, value)) %>%
#   deframe()
#
# #########################################
# ############### SAVE DATA ###############
# #########################################
# write_rds(lst(trajtypes, metrics, datasets_info, methods_info, norm_fun, mean_fun, mean_weights, dataset_source_weights), result_file("benchmark_results_input.rds"), compress = "xz")
# write_rds(lst(raw_data, metrics), result_file("benchmark_results_unnormalised.rds"), compress = "xz")
#
#
# ###################################################
# ############### CREATE AGGREGATIONS ###############
# ###################################################
benchmark_results_input <- read_rds(result_file("benchmark_results_input.rds", "07-benchmark"))

out <- benchmark_aggregate(
  data = raw_data %>% mutate(method_name = method_id),
  metrics = metrics,
  norm_fun = benchmark_results_input$norm_fun,
  mean_fun = benchmark_results_input$mean_fun,
  mean_weights = benchmark_results_input$mean_weights,
  dataset_source_weights = benchmark_results_input$dataset_source_weights
)


#####################################################
############### CALCULATE VARIABILITY ###############
#####################################################

worst_var <- var(c(rep(0, floor(num_bootstraps / 2)), rep(1, ceiling(num_bootstraps / 2))))

norm_var <- function(x) {
  maxx <- max(x)

  if (maxx == 0) return(1)

  worst_var_this <- worst_var * maxx ^ 2

  (worst_var_this - var(x) ) / worst_var_this
}

met2 <- c(metrics, "overall")

out$data_var <-
  out$data %>%
  select(method_id, method_name, dataset_id, param_id, !!met2) %>%
  group_by(method_id, method_name, dataset_id, param_id) %>%
  summarise_if(is.numeric, norm_var) %>%
  ungroup() %>%
  group_by(method_id, method_name, param_id) %>%
  summarise_if(is.numeric, mean) %>%
  ungroup() %>%
  gather(metric, value, !!met2) %>%
  mutate(metric = paste0("var_", metric))
#
# #####################################################
# ############### SAVE DATA ###############
# #####################################################
#
# write_rds(out, result_file("benchmark_results_normalised.rds"), compress = "xz")
#
