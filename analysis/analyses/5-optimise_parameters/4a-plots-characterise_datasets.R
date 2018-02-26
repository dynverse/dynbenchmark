library(dynalysis)
library(tidyverse)
library(dynplot)

experiment("5-optimise_parameters/4-plots")


list2env(read_rds(derived_file("outputs_postprocessed.rds", "5-optimise_parameters/3-evaluate_parameters")), environment())


############### COMPARISON OF SCORES BETWEEN DATASETS ###############
outputs_ind_dataset <- outputs_ind %>%
  group_by(method_short_name) %>%
  mutate(
    rank_correlation = percent_rank(correlation),
    rank_rf_mse = percent_rank(-rf_mse),
    rank_rf_rsq = percent_rank(rf_rsq),
    rank_edge_flip = percent_rank(edge_flip)
  ) %>%
  ungroup()

# aggregate over replicates
outputs_summrepl_dataset <- outputs_ind_dataset %>%
  group_by(method_name, method_short_name, task_id, paramset_id, task_source, trajectory_type, trajectory_type_f) %>%
  summarise_if(is.numeric, funs(mean, var)) %>%
  ungroup() %>%
  mutate(
    harm_mean = apply(cbind(rank_correlation_mean, rank_edge_flip_mean, rank_rf_mse_mean), 1, psych::harmonic.mean)
  )

# process overall evaluation
outputs_summ_dataset <- outputs_summrepl_dataset %>%
  group_by(task_id, trajectory_type, trajectory_type_f, task_source) %>%
  mutate(n = n()) %>%
  summarise_if(is.numeric, mean) %>%
  ungroup()

# get ordering of methods
dataset_ord <- outputs_summ_dataset %>%
  arrange(desc(harm_mean)) %>%
  .$task_id

# create method_name_f factor in all data structures
outputs_summ_dataset <- outputs_summ_dataset %>% mutate(task_id_f = factor(task_id, levels = rev(dataset_ord)))

dataset_comp <-
  outputs_summ_dataset %>%
  select(task_id, task_id_f, trajectory_type, trajectory_type_f, task_source, harm_mean, pct_errored_mean, rank_correlation_mean, rank_rf_mse_mean, rank_edge_flip_mean) %>%
  gather(metric, score, -task_id:-task_source) %>%
  mutate(metric_f = factor(metric, levels = c("harm_mean", "pct_errored_mean","rank_correlation_mean", "rank_edge_flip_mean", "rank_rf_mse_mean")))

##### create custom plot #####

# collect score boxes
scores <-
  dataset_comp %>%
  mutate(
    xmin = (as.integer(metric_f) - 1) * 1.05,
    xmax = xmin + score,
    ymin = (as.integer(task_id_f) - 1) * 1.05,
    ymax = ymin + 1
  )

# construct axis lines
metrics <- dataset_comp %>%
  group_by(metric_f) %>%
  summarise() %>%
  mutate(
    x = (as.integer(metric_f) - 1) * 1.05,
    xend = x + 1,
    y = -1,
    yend = -1
  )

# construct ticks
ticks_at <- seq(0, 1, length.out = 5)
ticks <- metrics %>%
  crossing(ticks_at) %>%
  mutate(
    x = (1 - ticks_at) * x + ticks_at * xend,
    yend = y - 1
  ) %>%
  select(-xend)

labels <- ticks %>%
  filter(ticks_at %in% c(0, 1)) %>%
  mutate(
    y = yend - 1,
    label = as.character(ticks_at)
  ) %>%
  select(-yend)

# construct axis labels
metric_nice_labels <- c(
  "harm_mean" = "Harmonic mean",
  "pct_errored_mean" = "Percentage errored",
  "rank_correlation_mean" = "Mean rank of correlation",
  "rank_edge_flip_mean" = "Mean rank of edge flip",
  "rank_rf_mse_mean" = "Mean rank of RF MSE"
)
metric_labels <- ticks %>%
  filter(ticks_at == .5) %>%
  mutate(
    y = yend - 2,
    label = metric_nice_labels[metric_f]
  ) %>%
  select(-yend)

# construct dataset meta information
dataset_info <-
  dataset_comp %>%
  filter(metric == "harm_mean") %>%
  select(-metric:-metric_f)
task_sources <- unique(dataset_info$task_source)
task_source_colours <- RColorBrewer::brewer.pal(length(task_sources), "Set1") %>% setNames(task_sources)
trajectory_type_colours <- setNames(trajtypes$color, trajtypes$id)

dataset_info_boxes <- bind_rows(
  dataset_info %>% mutate(
    xmin = -.05,
    xmax = -.25,
    ymin = (as.integer(task_id_f) - 1) * 1.05,
    ymax = ymin + 1,
    colour = trajectory_type_colours[trajectory_type]
  ),
  dataset_info %>% mutate(
    xmin = -.3,
    xmax = -.5,
    ymin = (as.integer(task_id_f) - 1) * 1.05,
    ymax = ymin + 1,
    colour = task_source_colours[task_source]
  )
)

metric_labels2 <-
  metric_labels %>%
  add_row(x = -.15, y = -4, label = "Trajectory\ntype") %>%
  add_row(x = -.4, y = -4, label = "Task\ngroup")

guides <-
  dataset_info %>%
  filter(as.integer(task_id_f) %% 5 == 1) %>%
  mutate(
    x = -.5,
    xend = metrics$xend %>% max,
    y = (as.integer(task_id_f) - 1) * 1.05 - .025,
    yend = y
  )

# construct plot
g <- ggplot() +
  geom_segment(aes(x = x, xend = xend, y = y, yend = yend), guides, colour = "lightgray", size = .2) +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), scores) +
  geom_segment(aes(x = x, xend = xend, y = y, yend = yend), metrics) +
  geom_segment(aes(x = x, xend = x, y = y, yend = yend), ticks) +
  geom_text(aes(x = x, y = y, label = label), labels, vjust = "top", hjust = "middle") +
  geom_text(aes(x = x, y = y, label = label), metric_labels2, vjust = "top", hjust = "middle") +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = colour), dataset_info_boxes) +
  cowplot::theme_nothing() +
  scale_fill_identity() +
  coord_cartesian(expand = T)

ggsave(figure_file("dataset_difficulty_leftside.pdf"), g, width = 16, height = 20)


task_source_levels <- c("real", "synthetic", "toy")
task_source_nice <- c("real" = "Real", "synthetic" = "Synthetic", "toy" = "Toy")
outputs_summ_dataset <- outputs_summ_dataset %>% mutate(task_source_f = factor(task_source, levels = rev(task_source_levels)))

# get ordering of methods
dataset_ord <- outputs_summ_dataset %>%
  arrange(task_source_f, desc(harm_mean)) %>%
  .$task_id

# create method_name_f factor in all data structures
outputs_summ_dataset <- outputs_summ_dataset %>% mutate(task_id_f = factor(task_id, levels = rev(dataset_ord)))

sel_metrics <- c("harm_mean", "pct_errored_mean","rank_correlation_mean", "rank_edge_flip_mean", "rank_rf_mse_mean")
dataset_comp <-
  outputs_summ_dataset %>%
  select(task_id, task_id_f, trajectory_type, trajectory_type_f, task_source, task_source_f, one_of(sel_metrics)) %>%
  gather(metric, score, -task_id:-task_source_f) %>%
  mutate(metric_f = factor(metric, levels = sel_metrics))


# collect score boxes
scores <-
  dataset_comp %>%
  group_by(metric_f) %>%
  arrange(task_id_f) %>%
  mutate(
    yheight = 1,
    ydiff = c(FALSE, diff(as.integer(task_source_f)) != 0),
    ygap = ifelse(ydiff, 1.05, .05),
    xmin = (as.integer(metric_f) - 1) * 1.05,
    xmax = xmin + score,
    ymin = cumsum(yheight + ygap),
    ymax = ymin + yheight
  ) %>%
  ungroup() %>%
  mutate(
    ymin = ymin - ymin[[1]],
    ymax = ymin + yheight
  )

guides <-
  scores %>%
  filter(metric == "harm_mean") %>%
  group_by(task_source) %>%
  arrange(task_id_f) %>%
  filter(seq_len(n()) %% 5 == 1) %>%
  mutate(
    x = 0,
    xend = metrics$xend %>% max,
    y = ymin - .025,
    yend = y
  )

task_sources <-
  scores %>%
  filter(metric == "harm_mean") %>%
  group_by(task_source, task_source_f) %>%
  arrange(task_id_f) %>%
  summarise(
    x = -.1,
    ymin = min(ymin),
    ymax = max(ymax),
    y = (ymin + ymax) / 2
  ) %>%
  ungroup()

g <- ggplot() +
  geom_segment(aes(x = x, xend = xend, y = y, yend = yend), guides, colour = "lightgray", size = .2) +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = trajectory_type), scores) +
  geom_segment(aes(x = x, xend = xend, y = y, yend = yend), metrics) +
  geom_segment(aes(x = x, xend = x, y = y, yend = yend), ticks) +
  geom_text(aes(x = x, y = y, label = label), labels, vjust = "top", hjust = "middle") +
  geom_text(aes(x = x, y = y, label = label), metric_labels, nudge_y = 1, vjust = "top", hjust = "middle", size = 5) +
  geom_segment(aes(x = x, xend = x, y = ymin, yend = ymax), task_sources) +
  geom_text(aes(x = x, y = y, label = task_source_nice[task_source]), task_sources, size = 5, vjust = "center", hjust = "right", nudge_x = -.05) +
  cowplot::theme_nothing() +
  scale_fill_manual(values = trajectory_type_colours, label=label_long) +
  coord_cartesian(
    expand = F,
    xlim = c(-.5, max(metrics$xend)+.1),
    ylim = c(-4.5, max(scores$ymax))
  ) +
  theme(
    legend.position = "right"
  ) +
  labs(fill = "Trajectory type")

g
ggsave(figure_file("dataset_difficulty_filled.pdf"), g, width = 20, height = 20)


### difficulty
out_summtraj_dataset <- outputs_summrepl_dataset %>%
  group_by(trajectory_type, task_source) %>%
  summarise_if(is.numeric, mean) %>%
  ungroup() %>%
  arrange(desc(harm_mean))


trajlevels <-
  out_summtraj_dataset %>%
  filter(task_source == "synthetic") %>%
  arrange(desc(harm_mean)) %>%
  .$trajectory_type

out_summtraj_dataset <- out_summtraj_dataset %>% mutate(
  trajectory_type_f = factor(trajectory_type, levels = trajlevels)
)

g <- ggplot(out_summtraj_dataset %>% gather(metric, score, harm_mean, one_of(sel_metrics)) %>% mutate(metric_f = factor(metric, levels = sel_metrics))) +
  geom_bar(aes(fct_rev(trajectory_type_f), score, fill = trajectory_type_f), stat = "identity") +
  facet_grid(task_source~metric_f) +
  scale_fill_manual(values = trajectory_type_colours, label=label_long) +
  cowplot::theme_cowplot() +
  coord_flip() +
  labs(x = NULL, y = NULL) +
  theme(legend.position = "none")
ggsave(figure_file("dataset_difficulty_grouped.pdf"), g, width = 16, height = 6)


############### COMPARISON OF EXECUTION TIMES ###############
step_levels <- c("sessionsetup", "preprocessing", "method", "postprocessing", "wrapping", "sessioncleanup", "cellwaypoints", "waypointedgeodesic", "correlation",
                 "mantel", "rf", "edge_flip")

time_ind <-
  outputs_ind_dataset %>%
  select(method_name, task_id, pct_errored, error_message, trajectory_type_f, starts_with("time_")) %>%
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
  group_by(task_id_f, step_f) %>%
  summarise(time = mean(time, na.rm = T)) %>%
  ungroup() %>%
  ggplot() +
  geom_bar(aes(task_id_f, time, fill = step_f), stat = "identity", position = position_stack(reverse = TRUE)) +
  scale_fill_brewer(palette = "Set3") +
  cowplot::theme_cowplot() +
  coord_flip() +
  labs(x = NULL, fill = "Time step")
ggsave(figure_file("dataset_timeperstep_overall.pdf"), g, width = 20, height = 10)

rm(g, time_ind, timeind_task_ord, timeind_meth_ord)


