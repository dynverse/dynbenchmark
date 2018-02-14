library(dynalysis)
library(tidyverse)
library(dynplot)

experiment("5-optimise_parameters/3-evaluate_parameters")

# Rsync latest results!
# PRISM:::rsync_remote(
#   remote_src = "prism",
#   path_src = paste0("/group/irc/shared/dynalysis/analysis/data/derived_data/", getOption("dynalysis_experiment_id"), "/"),
#   remote_dest = "",
#   path_dest = derived_file()
# )

############################################################
############### PART THREE: GENERATE FIGURES ###############
############################################################

outputs_list <- read_rds(derived_file("outputs_postprocessed.rds"))
chosen_task_source <- "mean_notoy"

# get ordering of methods
method_ord <- outputs_list$outputs_summtrajtype_totalsx2 %>%
  filter(task_source == chosen_task_source, trajectory_type == "overall") %>%
  arrange(desc(harm_mean)) %>%
  .$method_name

# create method_name_f factor in all data structures
for (oname in str_subset(names(outputs_list), "outputs")) {
  outputs_list[[oname]] <- outputs_list[[oname]] %>% mutate(method_name_f = factor(method_name, levels = rev(method_ord)))
}

# load all outputs in environment and remove outputs_list
list2env(outputs_list, environment())
rm(outputs_list)

# collect which methods use which prior information
prior_df <- outputs_ind %>% select(method_name, prior_str) %>% distinct()


############### OVERALL COMPARISON ###############
overall_comp <-
  outputs_summtrajtype_totalsx2 %>%
  filter(task_source == chosen_task_source, trajectory_type_f == "overall") %>%
  select(method_name, method_short_name, method_name_f, harm_mean, pct_errored_mean, rank_correlation_mean, rank_correlation_var, rank_rf_mse_mean, rank_rf_mse_var, rank_edge_flip_mean, rank_edge_flip_var, time_method_mean) %>%
  gather(metric, score, -method_name:-method_name_f) %>%
  mutate(metric_f = factor(metric, levels = c("harm_mean", "pct_errored_mean", "time_method_mean", "rank_correlation_mean", "rank_edge_flip_mean", "rank_rf_mse_mean", "rank_correlation_var", "rank_edge_flip_var", "rank_rf_mse_var")))

pdf(figure_file("1_overall_comparison.pdf"), 12, 12)
ggplot(overall_comp) +
  geom_bar(aes(method_name_f, score, fill = metric_f), stat = "identity") +
  facet_wrap(~metric_f, scales = "free", nrow = 3) +
  coord_flip() +
  theme_bw() +
  labs(x = NULL, y = NULL, fill = "Metric") +
  theme(legend.position = "none")

ggplot(overall_comp %>% left_join(prior_df, by = "method_name")) +
  geom_bar(aes(method_name_f, score, fill = prior_str), stat = "identity") +
  facet_wrap(~metric_f, scales = "free", nrow = 3) +
  coord_flip() +
  theme_bw() +
  scale_fill_brewer(palette = "Dark2") +
  theme(legend.position = "bottom") +
  labs(x = NULL, y = NULL, fill = "Prior")
dev.off()

rm(overall_comp)


############### COMPARISON PER TRAJECTORY TYPE ###############
pdf(figure_file("2_trajtype_comparison.pdf"), 20, 16)
ggplot(outputs_summtrajtype_totalsx2) +
  geom_point(aes(method_name_f, harm_mean)) +
  coord_flip() +
  theme_bw() +
  facet_grid(task_source~trajectory_type_f) +
  labs(
    x = NULL
  )

ggplot(outputs_summtrajtype_totalsx2) +
  geom_point(aes(method_name_f, rank_correlation_mean)) +
  coord_flip() +
  theme_bw() +
  facet_grid(task_source~trajectory_type_f) +
  labs(
    x = NULL
  )


ggplot(outputs_summtrajtype_totalsx2) +
  geom_point(aes(method_name_f, rank_edge_flip_mean)) +
  coord_flip() +
  theme_bw() +
  facet_grid(task_source~trajectory_type_f) +
  labs(
    x = NULL
  )

ggplot(outputs_summtrajtype_totalsx2) +
  geom_point(aes(method_name_f, rank_rf_mse_mean)) +
  coord_flip() +
  theme_bw() +
  facet_grid(task_source~trajectory_type_f) +
  labs(
    x = NULL
  )

ggplot(outputs_summtrajtype_totalsx2) +
  geom_point(aes(method_name_f, pct_errored_mean)) +
  coord_flip() +
  theme_bw() +
  facet_grid(task_source~trajectory_type_f) +
  labs(
    x = NULL
  )

dev.off()




############### COMPARISON OF EXECUTION TIMES ###############
step_levels <- c("sessionsetup", "preprocessing", "method", "postprocessing", "wrapping", "sessioncleanup", "geodesic", "correlation",
                 "coranking", "mantel", "rf", "edge_flip")

time_ind <-
  outputs_ind %>%
  select(method_name, method_name_f, task_id, pct_errored, error_message, trajectory_type_f, starts_with("time_")) %>%
  gather(step, time, starts_with("time")) %>%
  mutate(
    step = gsub("time_", "", step),
    step_f = factor(step, levels = step_levels)
  )

timeind_task_ord <- time_ind %>%
  group_by(task_id) %>%
  summarise(time = sum(time, na.rm = T)) %>%
  arrange(desc(time))

timeind_meth_ord <- time_ind %>%
  group_by(method_name, step_f) %>%
  summarise(time = mean(time, na.rm = T)) %>%
  summarise(time = sum(time, na.rm=T)) %>%
  arrange(desc(time))

time_ind <- time_ind %>% mutate(
  task_id_f = factor(task_id, levels = timeind_task_ord$task_id),
  method_name_f = factor(method_name, levels = timeind_meth_ord$method_name)
)

g <- time_ind %>%
  group_by(method_name_f, step_f) %>%
  summarise(time = mean(time, na.rm = T)) %>%
  ungroup() %>%
  ggplot() +
  geom_bar(aes(method_name_f, time, fill = step_f), stat = "identity", position = position_stack(reverse = TRUE)) +
  scale_fill_brewer(palette = "Set3") +
  cowplot::theme_cowplot() +
  coord_flip() +
  labs(x = NULL, fill = "Time step")
ggsave(figure_file("3_timeperstep_overall.pdf"), g, width = 10, height = 5)

g <- time_ind %>%
  group_by(trajectory_type_f, method_name, step_f) %>%
  summarise(time = mean(time, na.rm = T)) %>%
  ungroup() %>%
  ggplot() +
  geom_bar(aes(trajectory_type_f, time, fill = step_f), stat = "identity", position = position_stack(reverse = TRUE)) +
  facet_wrap(~method_name, scales = "free") +
  scale_fill_brewer(palette = "Set3") +
  cowplot::theme_cowplot() +
  coord_flip() +
  labs(x = NULL, fill = "Time step")
ggsave(figure_file("3_timeperstep_pertrajtype.pdf"), g, width = 20, height = 8)

rm(g, time_ind, timeind_task_ord, timeind_meth_ord)





############### COMPARISON OF SCORES BETWEEN TASK GROUPS ###############
out_gath <- outputs_summtrajtype %>%
  select(method_name, method_short_name, method_name_f, task_source, trajectory_type, trajectory_type_f, rank_correlation_mean, rank_rf_mse_mean, rank_edge_flip_mean, harm_mean) %>%
  gather(metric, value, rank_correlation_mean, rank_rf_mse_mean, rank_edge_flip_mean, harm_mean) %>%
  spread(task_source, value)

one <-
  ggplot(out_gath) +
  geom_point(aes(real, synthetic, colour = trajectory_type)) +
  coord_equal() +
  theme_bw() +
  scale_colour_brewer(palette = "Dark2") +
  facet_wrap(~metric, nrow = 1)
two <-
  ggplot(out_gath) +
  geom_point(aes(real, toy, colour = trajectory_type)) +
  coord_equal() +
  theme_bw() +
  scale_colour_brewer(palette = "Dark2") +
  facet_wrap(~metric, nrow = 1)
three <-
  ggplot(out_gath) +
  geom_point(aes(synthetic, toy, colour = trajectory_type)) +
  coord_equal() +
  theme_bw() +
  scale_colour_brewer(palette = "Dark2") +
  facet_wrap(~metric, nrow = 1)
cowplot::plot_grid(one, two, three, ncol = 1)



out_gath <- outputs_summmethod %>%
  select(method_name, method_short_name, method_name_f, task_source, rank_correlation_mean, rank_rf_mse_mean, rank_edge_flip_mean, harm_mean) %>%
  gather(metric, value, rank_correlation_mean, rank_rf_mse_mean, rank_edge_flip_mean, harm_mean) %>%
  spread(task_source, value)

one <-
  ggplot(out_gath) +
  geom_point(aes(real, synthetic)) +
  coord_equal() +
  theme_bw() +
  scale_colour_brewer(palette = "Dark2") +
  facet_wrap(~metric, nrow = 1)
two <-
  ggplot(out_gath) +
  geom_point(aes(real, toy)) +
  coord_equal() +
  theme_bw() +
  scale_colour_brewer(palette = "Dark2") +
  facet_wrap(~metric, nrow = 1)
three <-
  ggplot(out_gath) +
  geom_point(aes(synthetic, toy)) +
  coord_equal() +
  theme_bw() +
  scale_colour_brewer(palette = "Dark2") +
  facet_wrap(~metric, nrow = 1)
cowplot::plot_grid(one, two, three, ncol = 1)

rm(out_gath, one, two, three)
