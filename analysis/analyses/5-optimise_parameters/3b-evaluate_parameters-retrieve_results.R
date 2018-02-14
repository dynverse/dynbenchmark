library(dynalysis)
library(tidyverse)

experiment("5-optimise_parameters/3-evaluate_parameters")

##########################################################
############### PART TWO: RETRIEVE RESULTS ###############
##########################################################

# fetch results from cluster
bs_fetch_results(derived_file("suite/"))

# bind results in one data frame (without models)
outputs <- bs_bind_results(derived_file("suite/"), load_models = FALSE)

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

###################################################
############### CREATE AGGREGATIONS ###############
###################################################
list2env(read_rds(derived_file("config.rds")), environment())
tasks_info <- tasks %>% select(task_id = id, type, trajectory_type, task_group)
rm(tasks)

outputs_ind <- outputs %>%
  filter(!is.na(task_id)) %>%
  left_join(tasks_info, by = "task_id") %>%
  mutate(
    param_group = c("default", "optimised")[param_i],
    pct_errored = (error_message != "") + 0,
    prior_str = sapply(prior_df, function(prdf) ifelse(is.null(prdf) || nrow(prdf) == 0, "", paste(prdf$prior_names, collapse = ";"))),
    trajectory_type_f = factor(trajectory_type, levels = dynalysis::trajectory_types$id)
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
  group_by(method_name, method_short_name, task_id, fold_type, fold_i, group_sel, param_i, iteration_i, type, trajectory_type, task_group, param_group, prior_str, trajectory_type_f) %>%
  summarise_if(is.numeric, funs(mean, var)) %>%
  ungroup() %>%
  mutate(
    harm_mean = apply(cbind(rank_correlation_mean, rank_edge_flip_mean, rank_rf_mse_mean), 1, psych::harmonic.mean)
  )

# process trajtype grouped evaluation
outputs_summtrajtype <- outputs_summrepl %>%
  group_by(method_name, method_short_name, task_group, param_group, trajectory_type, trajectory_type_f) %>%
  mutate(n = n()) %>%
  summarise_if(is.numeric, mean) %>%
  ungroup()

# process overall evaluation
outputs_summmethod <- outputs_summtrajtype %>%
  group_by(method_name, method_short_name, task_group, param_group) %>%
  mutate(n = n()) %>%
  summarise_if(is.numeric, mean) %>%
  ungroup()

# adding mean per trajtype
outputs_summtrajtype_totals <- bind_rows(
  outputs_summtrajtype,
  outputs_summtrajtype %>%
    filter(task_group != "toy") %>%
    group_by(method_name, method_short_name, param_group, trajectory_type, trajectory_type_f) %>%
    summarise_if(is.numeric, mean) %>%
    ungroup() %>%
    mutate(task_group = "mean_notoy"),
  outputs_summtrajtype %>%
    group_by(method_name, method_short_name, param_group, trajectory_type, trajectory_type_f) %>%
    summarise_if(is.numeric, mean) %>%
    ungroup() %>%
    mutate(task_group = "mean_withtoy")
)

# adding mean per method
outputs_summmethod_totals <-
  bind_rows(
    outputs_summmethod,
    outputs_summmethod %>%
      filter(task_group != "toy") %>%
      group_by(method_name, method_short_name, param_group) %>%
      summarise_if(is.numeric, mean) %>%
      ungroup() %>%
      mutate(task_group = "mean_notoy"),
    outputs_summmethod %>%
      group_by(method_name, method_short_name, param_group) %>%
      summarise_if(is.numeric, mean) %>%
      ungroup() %>%
      mutate(task_group = "mean_withtoy")
  )

# combine all aggregated data frames
outputs_summtrajtype_totalsx2 <- bind_rows(
  outputs_summmethod_totals %>% mutate(trajectory_type = "overall"),
  outputs_summtrajtype_totals
) %>%
  mutate(trajectory_type_f = factor(trajectory_type, levels = c("overall", dynalysis::trajectory_types$id)))

# save data structures
to_save <- environment() %>% as.list()
to_save <- to_save[str_detect(names(to_save), "^outputs_")]
write_rds(to_save, derived_file("outputs_postprocessed.rds"))

# Upload ---------------------
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
method_names <- list.dirs(derived_file("suite"), recursive = FALSE, full.names = FALSE)
for (method_name in method_names) {
  cat("Syncing method output for ", method_name, "\n", sep = "")
  path_dest <- paste0("/group/irc/shared/dynalysis/analysis/data/derived_data/", getOption("dynalysis_experiment_id"), "/suite/", method_name, "/")

  if (length(list.files(paste0(derived_file("suite/"), method_name, "/"), pattern = "output_*")) > 0) {
    PRISM:::mkdir_remote(path_dest, remote = "prism")
    PRISM:::rsync_remote(
      remote_dest = "prism",
      path_dest = path_dest,
      remote_src = "",
      path_src = paste0(derived_file("suite/"), method_name, "/output_*.rds")
    )
  }
}
