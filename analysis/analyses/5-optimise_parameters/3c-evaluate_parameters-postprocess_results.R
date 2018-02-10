library(dynalysis)
library(tidyverse)
library(dynplot)

experiment("5-optimise_parameters/3-evaluate_parameters")

##########################################################
############### PART THREE: POST PROCESS #################
##########################################################

outputs <- read_rds(derived_file("outputs_without_models.rds"))
list2env(read_rds(derived_file("config.rds")), environment())

outputs <- outputs %>%
  filter(!is.na(task_id)) %>%
  left_join(tasks %>% select(task_id = id, type, trajectory_type, task_group), by = "task_id") %>%
  mutate(
    param_group = c("default", "optimised")[param_i],
    pct_errored = (error_message != "") + 0,
    prior_str = sapply(prior_df, function(prdf) ifelse(is.null(prdf) || nrow(prdf) == 0, "", paste(prdf$prior_names, collapse = ";"))),
    trajectory_type_f = factor(trajectory_type, levels = dynalysis:::trajectory_type_directed$name)
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
outputs_summrepl <- outputs %>%
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
  mutate(trajectory_type_f = factor(trajectory_type, levels = c("overall", dynalysis:::trajectory_type_directed$name)))

# save data structures
to_save <- environment() %>% as.list()
to_save <- to_save[str_detect(names(to_save), "^outputs")]
write_rds(to_save, derived_file("outputs_postprocessed.rds"))



