library(dynalysis)
library(tidyverse)

experiment("5-optimise_parameters/3-evaluate_parameters")

##########################################################
############### PART TWO: RETRIEVE RESULTS ###############
##########################################################

# fetch results from cluster
benchmark_fetch_results(derived_file("suite/"))

# bind results in one data frame (without models)
outputs <- benchmark_bind_results(derived_file("suite/"), load_models = FALSE)

# load tasks info
list2env(read_rds(derived_file("config.rds")), environment())
tasks_info <- map_df(
  paste0(local_tasks_folder, "/", task_ids, ".rds"),
  ~ read_rds(.) %>% select(task_id = id, trajectory_type, task_source)
)

# collect relevant trajectory types
trajtypes <-
  dynalysis::trajectory_types %>%
  filter(id %in% unique(tasks_info$trajectory_type)) %>%
  add_row(id = "overall", directedness = "directed", color = "#AAAAAA", background_color = "E6A1A1")

# print task errors
task_errors <- outputs %>%
  filter(is.na(task_id)) %>%
  mutate(error_message = str_sub(error_message, -600, -1)) %>%
  group_by(method_name, error_message) %>%
  summarise(n = n()) %>%
  ungroup()

print(task_errors)
write_tsv(task_errors, figure_file("errors_qsub.tsv"))

# print job errors
job_errors <- outputs %>%
  filter(error_message != "", !is.na(task_id)) %>%
  mutate(error_message = str_sub(error_message, -600, -1)) %>%
  group_by(method_name, error_message) %>%
  summarise(n = n(), example = task_id[[1]]) %>%
  ungroup() %>%
  arrange(desc(n))

print(job_errors)
write_tsv(job_errors, figure_file("errors_method.tsv"))

job_errors %>% filter(error_message %in% c("Memory limit exceeded"))
job_errors %>% filter(error_message %in% c("Time limit exceeded"))
job_errors %>% group_by(method_name) %>% summarise(n = sum(n), example = example[[1]]) %>% arrange(desc(n))

required_outputs <- length(task_ids) * num_repeats
outputs %>% filter(!is.na(task_id)) %>% group_by(method_name) %>% summarise(n = n()) %>% filter(n != required_outputs) %>% mutate(pass = n > .9 * required_outputs)

###################################################
############### CREATE AGGREGATIONS ###############
###################################################

outputs <- outputs %>%
  mutate(
    method_name = ifelse(method_short_name == "manual", paste0("manual by ", paramset_id), method_name),
    method_short_name = ifelse(method_short_name == "manual", paste0("manual_", paramset_id), method_short_name)
  )

error_message_interpret <- function(error_message) {
  map_chr(
    error_message,
    function(err) {
      if (grepl("MemoryError", err)) {
        "Memory limit exceeded"
      } else {
        err
      }
    }
  )
}

outputs_ind <- outputs %>%
  group_by(method_name) %>%
  ungroup() %>%
  left_join(tasks_info, by = "task_id") %>%
  filter(task_source != "toy") %>%
  mutate(
    error_message_int = error_message_interpret(error_message),
    pct_errored = (error_message_int != "") + 0,
    pct_time_exceeded = (error_message_int == "Time limit exceeded") + 0,
    pct_memory_exceeded = (error_message_int == "Memory limit exceeded") + 0,
    prior_str = sapply(prior_df, function(prdf) ifelse(is.null(prdf) || nrow(prdf) == 0, "", paste(prdf$prior_names, collapse = ";"))),
    trajectory_type_f = factor(trajectory_type, levels = trajtypes$id)
  ) %>%
  group_by(task_id) %>%
  mutate(
    rank_correlation = percent_rank(correlation),
    rank_rf_mse = percent_rank(-rf_mse),
    rank_rf_rsq = percent_rank(rf_rsq),
    rank_edge_flip = percent_rank(edge_flip)
  ) %>%
  ungroup()

# aggregate over replicates
outputs_summrepl <- outputs_ind %>%
  group_by(method_name, method_short_name, task_id, paramset_id, trajectory_type, task_source, prior_str, trajectory_type_f) %>%
  summarise_if(is.numeric, funs(mean, var)) %>%
  ungroup() %>%
  mutate(
    harm_mean = apply(cbind(rank_correlation_mean, rank_edge_flip_mean, rank_rf_mse_mean), 1, psych::harmonic.mean)
  )

# process trajtype grouped evaluation
outputs_summtrajtype <- outputs_summrepl %>%
  group_by(method_name, method_short_name, task_source, paramset_id, trajectory_type, trajectory_type_f) %>%
  mutate(n = n()) %>%
  summarise_if(is.numeric, mean) %>%
  ungroup()

# process overall evaluation
outputs_summmethod <- outputs_summtrajtype %>%
  group_by(method_name, method_short_name, task_source, paramset_id) %>%
  mutate(n = n()) %>%
  summarise_if(is.numeric, mean) %>%
  ungroup()

# adding mean per trajtype
outputs_summtrajtype_totals <- bind_rows(
  outputs_summtrajtype,
  outputs_summtrajtype %>%
    # filter(task_source != "toy") %>%
    group_by(method_name, method_short_name, paramset_id, trajectory_type, trajectory_type_f) %>%
    summarise_if(is.numeric, mean) %>%
    ungroup() %>%
    mutate(task_source = "mean")
  # outputs_summtrajtype %>%
  #   group_by(method_name, method_short_name, paramset_id, trajectory_type, trajectory_type_f) %>%
  #   summarise_if(is.numeric, mean) %>%
  #   ungroup() %>%
  #   mutate(task_source = "mean_withtoy")
)

# adding mean per method
outputs_summmethod_totals <-
  bind_rows(
    outputs_summmethod,
    outputs_summmethod %>%
      # filter(task_source != "toy") %>%
      group_by(method_name, method_short_name, paramset_id) %>%
      summarise_if(is.numeric, mean) %>%
      ungroup() %>%
      mutate(task_source = "mean")
    # outputs_summmethod %>%
    #   group_by(method_name, method_short_name, paramset_id) %>%
    #   summarise_if(is.numeric, mean) %>%
    #   ungroup() %>%
    #   mutate(task_source = "mean_withtoy")
  )

# combine all aggregated data frames
outputs_summtrajtype_totalsx2 <- bind_rows(
  outputs_summmethod_totals %>% mutate(trajectory_type = "overall"),
  outputs_summtrajtype_totals
) %>%
  mutate(trajectory_type_f = factor(trajectory_type, levels = trajtypes$id))

# save data structures
to_save <- environment() %>% as.list()
to_save <- to_save[c(str_subset(names(to_save), "^outputs_"), "trajtypes")]
write_rds(to_save, derived_file("outputs_postprocessed.rds"))

# # Upload ---------------------
PRISM:::rsync_remote(
  remote_dest = "prism",
  path_dest = paste0("/group/irc/shared/dynalysis/analysis/data/derived_data/", getOption("dynalysis_experiment_id"), "/"),
  remote_src = "",
  path_src = derived_file("config.rds")
)
PRISM:::rsync_remote(
  remote_dest = "prism",
  path_dest = paste0("/group/irc/shared/dynalysis/analysis/data/derived_data/", getOption("dynalysis_experiment_id"), "/"),
  remote_src = "",
  path_src = derived_file("outputs_postprocessed.rds")
)
# method_names <- list.dirs(derived_file("suite"), recursive = FALSE, full.names = FALSE)
# for (method_name in method_names) {
#   cat("Syncing method output for ", method_name, "\n", sep = "")
#   path_dest <- paste0("/group/irc/shared/dynalysis/analysis/data/derived_data/", getOption("dynalysis_experiment_id"), "/suite/", method_name, "/")
#
#   if (length(list.files(paste0(derived_file("suite/"), method_name, "/"), pattern = "output_*")) > 0) {
#     PRISM:::mkdir_remote(path_dest, remote = "prism")
#     PRISM:::rsync_remote(
#       remote_dest = "prism",
#       path_dest = path_dest,
#       remote_src = "",
#       path_src = paste0(derived_file("suite/"), method_name, "/output_*.rds")
#     )
#   }
# }
