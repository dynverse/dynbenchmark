library(dynverse)
library(tidyverse)

experiment("5-optimise_parameters")

##########################################################
############### PART TWO: RETRIEVE RESULTS ###############
##########################################################

# fetch results from cluster
paramoptim_fetch_results(derived_file("suite/"))

# bind results in one data frame (without models)
outputs <- paramoptim_bind_results(derived_file("suite/"))

# load tasks info
list2env(read_rds(derived_file("config.rds")), environment())
tasks <- load_datasets()

###################################################
############### CREATE AGGREGATIONS ###############
###################################################

outputs_ind <- outputs %>%
  left_join(tasks_info, by = "task_id") %>%
  filter(task_source != "toy") %>%
  group_by(task_id) %>%
  mutate(
    rank_correlation = percent_rank(correlation),
    rank_rf_mse = percent_rank(-rf_mse),
    rank_rf_rsq = percent_rank(rf_rsq),
    rank_edge_flip = percent_rank(edge_flip),
    harm_mean = apply(cbind(rank_correlation, rank_edge_flip, rank_rf_mse), 1, psych::harmonic.mean)
  ) %>%
  ungroup()

# process trajtype grouped evaluation
outputs_summtrajtype <- outputs_ind %>%
  group_by(method_name, method_short_name, task_source, param_i, repeat_i, trajectory_type) %>%
  mutate(n = n()) %>%
  summarise_if(is.numeric, mean) %>%
  ungroup()

# process overall evaluation
outputs_summmethod <- outputs_summtrajtype %>%
  group_by(method_name, method_short_name, task_source, param_i, repeat_i) %>%
  mutate(n = n()) %>%
  summarise_if(is.numeric, mean) %>%
  ungroup()


# adding mean per method
outputs_summmethod_totals <-
    outputs_summmethod %>%
      group_by(method_name, method_short_name, param_i, repeat_i) %>%
      summarise_if(is.numeric, mean) %>%
      ungroup() %>%
      mutate(task_source = "mean")

ggplot(outputs_summmethod_totals) + geom_point(aes(param_i, harm_mean)) +
  facet_wrap(~method_short_name, scales = "free") +
  cowplot::theme_cowplot()
