library(dynalysis)
library(tidyverse)

experiment("6-benchmark")

##########################################################
############### PART TWO: RETRIEVE RESULTS ###############
##########################################################

# fetch results from cluster
benchmark_fetch_results(derived_file("suite/"))

# bind results in one data frame (without models)
outputs <- benchmark_bind_results(derived_file("suite/"), load_models = FALSE)

# load tasks info
not_list <- function(x) !is.list(x)
list2env(read_rds(derived_file("config.rds")), environment())
tasks_info <- map_df(
  paste0(local_tasks_folder, "/", task_ids, ".rds"),
  ~ read_rds(.) %>% select_if(not_list)
)
write_rds(tasks_info, result_file("tasks_info.rds"))

# collect relevant trajectory types
trajtypes <-
  dynalysis::trajectory_types %>%
  filter(id %in% unique(tasks_info$trajectory_type)) %>%
  add_row(id = "overall", directedness = "directed", color = "#AAAAAA", background_color = "E6A1A1", .before = 1)

# print task errors
task_errors <- outputs %>%
  filter(is.na(task_id)) %>%
  mutate(error_message = str_sub(error_message, -600, -1)) %>%
  group_by(method_name, error_message) %>%
  summarise(n = n()) %>%
  ungroup()

print(task_errors)
write_tsv(task_errors, derived_file("errors_qsub.tsv"))

# print job errors
job_errors <- outputs %>%
  filter(error_message != "", !is.na(task_id)) %>%
  mutate(error_message = str_sub(error_message, -600, -1)) %>%
  group_by(method_name, error_message) %>%
  summarise(n = n(), example = task_id[[1]]) %>%
  ungroup() %>%
  arrange(desc(n))

print(job_errors)
write_tsv(job_errors, derived_file("errors_method.tsv"))

job_errors %>% filter(error_message %in% c("Memory limit exceeded"))
job_errors %>% filter(error_message %in% c("Time limit exceeded"))
job_errors %>% group_by(method_name) %>% summarise(n = sum(n), example = example[[1]]) %>% arrange(desc(n))

required_outputs <- length(task_ids) * num_repeats
outputs %>% filter(!is.na(task_id)) %>% group_by(method_name) %>% summarise(n = n()) %>% filter(n != required_outputs) %>% mutate(pass = n > .9 * required_outputs)

###################################################
############### CREATE AGGREGATIONS ###############
###################################################


# filter control methods
outputs <- outputs %>% filter(!method_short_name %in% c("scorspar", "identity", "shuffle"))

# filter disconnected as there is only 1 dataset
outputs <- outputs %>% filter(task_id != "real/blastocyst-monkey_nakamura")
trajtypes <- trajtypes %>% filter(id != "disconnected_directed_graph")


error_message_interpret <- function(error_message) {
  map_chr(
    error_message,
    function(err) {
      if (grepl("MemoryError", err) | grepl("OOM when allocating", err)) {
        "Memory limit exceeded"
      } else {
        err
      }
    }
  )
}

scalesigmoid_trafo <- function (x, remove_errored = TRUE, max_scale = TRUE) {
  xn <- x
  if (remove_errored) xn <- xn[xn != 0]
  if (max_scale) {
    y <- (x - mean(xn)) / max(abs(xn - mean(xn))) * 5
  } else {
    y <- (x - mean(xn)) / var(xn) * 5
  }
  sigmoid::sigmoid(y)
}

# previously:
# trafo_fun <- percent_rank
trafo_fun <- scalesigmoid_trafo

outputs_ind <- outputs %>%
  left_join(tasks_info %>% select(task_id = id, trajectory_type, task_source), by = "task_id") %>%
  filter(task_source != "toy") %>%
  mutate(
    rf_mse_inv = 1 - rf_mse,
    error_message_int = error_message_interpret(error_message),
    time_method = ifelse(error_message_int == "Time limit exceeded", timeout_per_execution, time_method),
    pct_errored = (error_message_int != "") + 0,
    pct_time_exceeded = (error_message_int == "Time limit exceeded") + 0,
    pct_memory_exceeded = (error_message_int == "Memory limit exceeded") + 0,
    pct_other_error = pct_errored - pct_time_exceeded - pct_memory_exceeded,
    prior_str = sapply(prior_df, function(prdf) ifelse(is.null(prdf) || nrow(prdf) == 0, "", paste(prdf$prior_names, collapse = ";"))),
    trajectory_type_f = factor(trajectory_type, levels = trajtypes$id)
  ) %>%
  group_by(task_id) %>%
  mutate(
    norm_correlation = trafo_fun(correlation),
    norm_edge_flip = trafo_fun(edge_flip),
    norm_rf_mse = trafo_fun(rf_mse_inv),
    rank_time_method = percent_rank(-time_method)
    # rank_time_method = 1 - (time_method / max(time_method))
  ) %>%
  ungroup()

# aggregate over replicates
outputs_summrepl <- outputs_ind %>%
  group_by(method_name, method_short_name, task_id, paramset_id, trajectory_type, task_source, prior_str, trajectory_type_f) %>%
  summarise_if(is.numeric, mean) %>%
  ungroup() %>%
  mutate(
    pct_allerrored = (pct_other_error == 1)+0,
    pct_stochastic = pct_other_error - pct_allerrored,
    harm_mean = apply(cbind(norm_correlation, norm_edge_flip, norm_rf_mse), 1, psych::harmonic.mean)
  )

# process trajtype grouped evaluation
outputs_summtrajtype <- outputs_summrepl %>%
  group_by(method_name, method_short_name, task_source, paramset_id, trajectory_type, trajectory_type_f) %>%
  mutate(n = n()) %>%
  summarise_if(is.numeric, mean) %>%
  ungroup() %>%
  mutate(
    harm_mean = apply(cbind(norm_correlation, norm_edge_flip, norm_rf_mse), 1, psych::harmonic.mean)
  )

# process overall evaluation
outputs_summmethod <- outputs_summtrajtype %>%
  group_by(method_name, method_short_name, task_source, paramset_id) %>%
  mutate(n = n()) %>%
  summarise_if(is.numeric, mean) %>%
  ungroup() %>%
  mutate(
    harm_mean = apply(cbind(norm_correlation, norm_edge_flip, norm_rf_mse), 1, psych::harmonic.mean)
  )

# adding mean per trajtype
outputs_summtrajtype_totals <- bind_rows(
  outputs_summtrajtype,
  outputs_summtrajtype %>%
    group_by(method_name, method_short_name, paramset_id, trajectory_type, trajectory_type_f) %>%
    summarise_if(is.numeric, mean) %>%
    ungroup() %>%
    mutate(task_source = "mean")
) %>%
  mutate(
    harm_mean = apply(cbind(norm_correlation, norm_edge_flip, norm_rf_mse), 1, psych::harmonic.mean)
  )

# adding mean per method
outputs_summmethod_totals <-
  bind_rows(
    outputs_summmethod,
    outputs_summmethod %>%
      group_by(method_name, method_short_name, paramset_id) %>%
      summarise_if(is.numeric, mean) %>%
      ungroup() %>%
      mutate(task_source = "mean")
  ) %>%
  mutate(
    harm_mean = apply(cbind(norm_correlation, norm_edge_flip, norm_rf_mse), 1, psych::harmonic.mean)
  )

# combine all aggregated data frames
outputs_summtrajtype_totalsx2 <- bind_rows(
  outputs_summmethod_totals %>% mutate(trajectory_type = "overall"),
  outputs_summtrajtype_totals
) %>%
  mutate(trajectory_type_f = factor(trajectory_type, levels = trajtypes$id)) %>%
  mutate(
    harm_mean = apply(cbind(norm_correlation, norm_edge_flip, norm_rf_mse), 1, psych::harmonic.mean)
  )

# save data structures
to_save <- environment() %>% as.list()
to_save <- to_save[c(str_subset(names(to_save), "^outputs_"), "trajtypes")]
write_rds(to_save, derived_file("outputs_postprocessed.rds"))

# # # Upload ---------------------
# qsub:::rsync_remote(
#   remote_dest = "prism",
#   path_dest = paste0("/group/irc/shared/dynalysis/analysis/data/derived_data/", getOption("dynalysis_experiment_id"), "/"),
#   remote_src = "",
#   path_src = derived_file("config.rds")
# )
# qsub:::rsync_remote(
#   remote_dest = "prism",
#   path_dest = paste0("/group/irc/shared/dynalysis/analysis/data/derived_data/", getOption("dynalysis_experiment_id"), "/"),
#   remote_src = "",
#   path_src = derived_file("outputs_postprocessed.rds")
# )
# method_names <- list.dirs(derived_file("suite"), recursive = FALSE, full.names = FALSE)
# for (method_name in method_names) {
#   cat("Syncing method output for ", method_name, "\n", sep = "")
#   path_dest <- paste0("/group/irc/shared/dynalysis/analysis/data/derived_data/", getOption("dynalysis_experiment_id"), "/suite/", method_name, "/")
#
#   if (length(list.files(paste0(derived_file("suite/"), method_name, "/"), pattern = "output_*")) > 0) {
#     qsub:::mkdir_remote(path_dest, remote = "prism")
#     qsub:::rsync_remote(
#       remote_dest = "prism",
#       path_dest = path_dest,
#       remote_src = "",
#       path_src = paste0(derived_file("suite/"), method_name, "/output_*.rds")
#     )
#   }
# }
