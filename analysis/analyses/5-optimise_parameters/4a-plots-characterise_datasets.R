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
  group_by(method_name, method_short_name, task_id, fold_type, fold_i, group_sel, param_i, iteration_i, type, trajectory_type, task_group, param_group, prior_str, trajectory_type_f) %>%
  summarise_if(is.numeric, funs(mean, var)) %>%
  ungroup() %>%
  mutate(
    harm_mean = apply(cbind(rank_correlation_mean, rank_edge_flip_mean, rank_rf_mse_mean), 1, psych::harmonic.mean)
  )

# process overall evaluation
outputs_summ_dataset <- outputs_summrepl_dataset %>%
  group_by(task_id, trajectory_type, trajectory_type_f, task_group) %>%
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
  select(task_id, task_id_f, trajectory_type, trajectory_type_f, task_group, harm_mean, pct_errored_mean, rank_correlation_mean, rank_rf_mse_mean, rank_edge_flip_mean) %>%
  gather(metric, score, -task_id:-task_group) %>%
  mutate(metric_f = factor(metric, levels = c("harm_mean", "pct_errored_mean","rank_correlation_mean", "rank_edge_flip_mean", "rank_rf_mse_mean")))

ggplot(dataset_comp) +
  geom_bar(aes(task_id_f, score), stat = "identity") +
  facet_wrap(~metric_f, nrow = 1) +
  coord_flip() +
  cowplot::theme_cowplot() +
  labs(x = NULL, y = NULL, fill = "Metric") +
  scale_fill_brewer(palette = "Dark2")

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
task_groups <- unique(dataset_info$task_group)
task_group_colours <- RColorBrewer::brewer.pal(length(task_groups), "Set1") %>% setNames(task_groups)
trajectory_types <- levels(dataset_info$trajectory_type_f)
trajectory_type_colours <- RColorBrewer::brewer.pal(length(trajectory_types), "Dark2") %>% setNames(trajectory_types)

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
    colour = task_group_colours[task_group]
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


task_group_levels <- c("real", "synthetic", "toy")
task_group_nice <- c("real" = "Real", "synthetic" = "Synthetic", "toy" = "Toy")
outputs_summ_dataset <- outputs_summ_dataset %>% mutate(task_group_f = factor(task_group, levels = rev(task_group_levels)))

# get ordering of methods
dataset_ord <- outputs_summ_dataset %>%
  arrange(task_group_f, desc(harm_mean)) %>%
  .$task_id

# create method_name_f factor in all data structures
outputs_summ_dataset <- outputs_summ_dataset %>% mutate(task_id_f = factor(task_id, levels = rev(dataset_ord)))

dataset_comp <-
  outputs_summ_dataset %>%
  select(task_id, task_id_f, trajectory_type, trajectory_type_f, task_group, task_group_f, harm_mean, pct_errored_mean, rank_correlation_mean, rank_rf_mse_mean, rank_edge_flip_mean) %>%
  gather(metric, score, -task_id:-task_group_f) %>%
  mutate(metric_f = factor(metric, levels = c("harm_mean", "pct_errored_mean","rank_correlation_mean", "rank_edge_flip_mean", "rank_rf_mse_mean")))


# collect score boxes
scores <-
  dataset_comp %>%
  group_by(metric_f) %>%
  arrange(task_id_f) %>%
  mutate(
    yheight = 1,
    ydiff = c(FALSE, diff(as.integer(task_group_f)) != 0),
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
  group_by(task_group) %>%
  arrange(task_id_f) %>%
  filter(seq_len(n()) %% 5 == 1) %>%
  mutate(
    x = 0,
    xend = metrics$xend %>% max,
    y = ymin - .025,
    yend = y
  )

task_groups <-
  scores %>%
  filter(metric == "harm_mean") %>%
  group_by(task_group, task_group_f) %>%
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
  geom_segment(aes(x = x, xend = x, y = ymin, yend = ymax), task_groups) +
  geom_text(aes(x = x, y = y, label = task_group_nice[task_group]), task_groups, size = 5, vjust = "center", hjust = "right", nudge_x = -.05) +
  cowplot::theme_nothing() +
  scale_fill_manual(values = trajectory_type_colours) +
  coord_cartesian(
    expand = F,
    xlim = c(-1, max(metrics$xend)+.1),
    ylim = c(-4.5, max(scores$ymax))
  )

g
ggsave(figure_file("dataset_difficulty_filled.pdf"), g, width = 16, height = 20)
