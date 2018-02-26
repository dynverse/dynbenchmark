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
chosen_task_source <- "mean"
# chosen_task_source <- "real"

# get ordering of methods
method_ord <- outputs_list$outputs_summtrajtype_totalsx2 %>%
  filter(task_source == chosen_task_source, trajectory_type == "overall") %>%
  arrange(desc(harm_mean)) %>%
  .$method_name

# create method_name_f factor in all data structures
for (oname in str_subset(names(outputs_list), "outputs")) {
  outputs_list[[oname]] <- outputs_list[[oname]] %>%
    mutate(method_name_f = factor(method_name, levels = rev(method_ord)))
}

# load all outputs in environment and remove outputs_list
list2env(outputs_list, environment())
rm(outputs_list)

# collect which methods use which prior information
prior_df <- outputs_ind %>% select(method_name, prior_str) %>% distinct()


############### OVERALL COMPARISON ###############
metr_lev <- c(
  "harm_mean", "rank_correlation_mean", "rank_edge_flip_mean", "rank_rf_mse_mean",
  "real", "synthetic", "time_method_mean", "pct_errored_mean",
  "pct_time_exceeded_mean", "pct_memory_exceeded_mean", "num_setseed_calls_mean", "num_files_created_mean"
)

oc1 <-
  outputs_summtrajtype_totalsx2 %>%
  filter(task_source == c("real", "synthetic")) %>%
  select(method_name:paramset_id, metric = task_source, score = harm_mean)
oc2 <-
  outputs_summtrajtype_totalsx2 %>%
  filter(task_source == chosen_task_source, trajectory_type_f == "overall") %>%
  select(method_name, method_short_name, method_name_f, paramset_id, one_of(metr_lev)) %>%
  gather(metric, score, -method_name:-paramset_id)


overall_comp <-
  bind_rows(oc1, oc2) %>%
  mutate(metric_f = factor(metric, levels = metr_lev))

pdf(figure_file("1_overall_comparison.pdf"), 16, 12)
ggplot(overall_comp) +
  geom_bar(aes(method_name_f, score, fill = metric_f), stat = "identity") +
  facet_wrap(~metric_f, scales = "free", ncol = 4, labeller = label_facet()) +
  coord_flip() +
  theme_bw() +
  labs(x = NULL, y = NULL, fill = "Metric") +
  theme(legend.position = "none")

ggplot(overall_comp %>% left_join(prior_df, by = "method_name")) +
  geom_bar(aes(method_name_f, score, fill = prior_str), stat = "identity") +
  facet_wrap(~metric_f, scales = "free", ncol = 4, labeller = label_facet()) +
  coord_flip() +
  theme_bw() +
  scale_fill_brewer(palette = "Dark2") +
  theme(legend.position = "bottom") +
  labs(x = NULL, y = NULL, fill = "Prior")
dev.off()

rm(overall_comp)


############### COMPARISON PER TRAJECTORY TYPE ###############
lvls <- rev(levels(outputs_summtrajtype_totalsx2$method_name_f))
cols <- RColorBrewer::brewer.pal(8, "Dark2")
cols <- RColorBrewer::brewer.pal(4, "Blues")[2:4]
cols <- viridis::viridis(8)[-1]
method_cols <- rep(cols, ceiling(length(lvls) / length(cols)))[seq_along(lvls)]

pdf(figure_file("2_trajtype_comparison.pdf"), 20, 12)
ggplot(outputs_summtrajtype_totalsx2) +
  geom_point(aes(method_name_f, harm_mean, colour = method_name_f)) +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "none") +
  scale_colour_manual(values = method_cols) +
  facet_grid(task_source~trajectory_type_f) +
  labs(
    x = NULL
  )

ggplot(outputs_summtrajtype_totalsx2) +
  geom_point(aes(method_name_f, rank_correlation_mean, colour = method_name_f)) +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "none") +
  scale_colour_manual(values = method_cols) +
  facet_grid(task_source~trajectory_type_f) +
  labs(
    x = NULL
  )


ggplot(outputs_summtrajtype_totalsx2) +
  geom_point(aes(method_name_f, rank_edge_flip_mean, colour = method_name_f)) +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "none") +
  scale_colour_manual(values = method_cols) +
  facet_grid(task_source~trajectory_type_f) +
  labs(
    x = NULL
  )

ggplot(outputs_summtrajtype_totalsx2) +
  geom_point(aes(method_name_f, rank_rf_mse_mean, colour = method_name_f)) +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "none") +
  scale_colour_manual(values = method_cols) +
  facet_grid(task_source~trajectory_type_f) +
  labs(
    x = NULL
  )


dev.off()




############### COMPARISON OF EXECUTION TIMES ###############
step_levels <- c(
  "sessionsetup", "preprocessing", "method", "postprocessing", "wrapping", "sessioncleanup",
  "cellwaypoints", "waypointedgeodesic", "correlation", "mantel", "rf", "edge_flip"
)

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
  labs(x = NULL, y = NULL, fill = "Time step")
ggsave(figure_file("3_timeperstep_pertrajtype.pdf"), g, width = 20, height = 10)

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
one


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
one

rm(out_gath, one)
