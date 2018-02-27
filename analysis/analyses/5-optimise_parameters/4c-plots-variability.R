library(dynalysis)
library(tidyverse)
library(dynplot)

experiment("5-optimise_parameters/4-plots")

############################################################
############### PART THREE: GENERATE FIGURES ###############
############################################################

outputs_list <- read_rds(derived_file("outputs_postprocessed.rds", "5-optimise_parameters/3-evaluate_parameters"))
chosen_task_source <- "mean"

trajtypes <- outputs_list$trajtypes

# sets seeds
setseed <- outputs_list$outputs_ind %>%
  group_by(method_short_name) %>%
  summarise(sets_seeds = any(num_setseed_calls > 0)) %>%
  ungroup()

# aggregate over replicates
outputs_summrepl <- outputs_list$outputs_ind %>%
  group_by(method_name, method_short_name, task_id, paramset_id, trajectory_type, task_source, prior_str, trajectory_type_f) %>%
  summarise_if(is.numeric, var) %>%
  group_by(task_id) %>%
  ungroup() %>%
  rename_if(is.numeric, function(x) paste0("var_", x)) %>%
  mutate(
    mean_var = apply(cbind(var_rank_correlation, var_rank_edge_flip, var_rank_rf_mse), 1, mean)
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

# adding mean per method
outputs_summmethod_totals <-
  outputs_summmethod %>%
  group_by(method_name, method_short_name, paramset_id) %>%
  summarise_if(is.numeric, mean) %>%
  ungroup()


vardf <-
  outputs_summmethod_totals %>%
  arrange(mean_var) %>%
  mutate(
    method_name_f = factor(method_name, levels = method_name)
  ) %>%
  left_join(setseed, by = "method_short_name")

plot_metrics <- c("mean_var", "var_rank_correlation", "var_rank_edge_flip", "var_rank_rf_mse")
vardf_g <- vardf %>%
  select(method_name, method_name_f, method_short_name, sets_seeds, one_of(plot_metrics)) %>%
  gather(metric, score, one_of(plot_metrics))

g <- ggplot(vardf_g) +
  geom_bar(aes(fct_rev(method_name_f), score, fill = metric, alpha = sets_seeds), stat = "identity") +
  coord_flip() +
  facet_wrap(~metric, nrow = 1) +
  cowplot::theme_cowplot() +
  theme(legend.position = "none") +
  labs(x = NULL, y = NULL) +
  scale_alpha_manual(values = c("TRUE" = .3, "FALSE" = 1))

g

ggsave(figure_file("variance_ranking.pdf"), g, width = 12, height = 6)



write_rds(lst(vardf), derived_file("variance_results.rds"))




