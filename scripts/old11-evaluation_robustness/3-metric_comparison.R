library(tidyverse)
library(dynbenchmark)

experiment("11-evaluation_robustness")

methods <- read_rds(derived_file("methods.rds", experiment_id = "04-method_characterisation")) %>%
  filter(type %in% c("algorithm", "control"))
outputs_list <- read_rds(derived_file("outputs_postprocessed.rds", "06-optimise_parameters/3-evaluate_parameters"))

# create metric scores
trajtype_metric_scores <- outputs_list$outputs_summtrajtype_totalsx2 %>%
  filter(dataset_source == "mean") %>%
  rename(method_id = method_short_name) %>%
  select(method_id, trajectory_type = trajectory_type_f, one_of(names(metrics_sel))) %>%
  gather(metric, score, one_of(names(metrics_sel))) %>%
  mutate(metric_lab = metrics_sel[metric]) %>%
  drop_na()

cross_df <- crossing(metric_left = names(metrics_sel), metric_right = names(metrics_sel)) %>%
  filter(metric_left < metric_right) %>%
  left_join(trajtype_metric_scores %>% rename(metric_left = metric, metric_lab_left = metric_lab, score_left = score), by = c("metric_left")) %>%
  left_join(trajtype_metric_scores %>% rename(metric_right = metric, metric_lab_right = metric_lab, score_right = score), by = c("metric_right", "method_id", "trajectory_type")) %>%
  mutate(
    facet_metric = paste0(metric_lab_left, " vs ", metric_lab_right),
    trajectory_type_l = factor(label_long(trajectory_type), levels = label_long(levels(trajectory_type)))
  )

g <- ggplot(cross_df) +
  geom_abline(intercept = 0, slope = 1) +
  geom_point(aes(score_right, score_left, colour = trajectory_type)) +
  facet_grid(facet_metric ~ trajectory_type_l) +
  theme_bw() +
  coord_equal() +
  labs(x = "Score B (see right facet label)", y = "Score A (see left facet label)") +
  scale_color_manual(values = set_names(c("black", "black", trajectory_types$colour), c("all", "overall", trajectory_types$id))) +
  theme(legend.position = "none", panel.grid = element_blank()) +
  scale_x_continuous(breaks = c(0, 1)) +
  scale_y_continuous(breaks = c(0, 1))
g

ggsave(figure_file("metrics_comparison.svg"), g, width = 14, height = 5)
write_rds(g, figure_file("metrics_comparison.rds"))

df <- outputs_list$outputs_summtrajtype_totalsx2 %>%
  filter(dataset_source == "mean") %>%
  mutate(
    trajectory_type_l = factor(label_long(trajectory_type_f), levels = label_long(levels(trajectory_type_f)))
  )

h <- ggplot(df) +
  geom_point(aes(pct_errored, harm_mean, colour = trajectory_type)) +
  facet_wrap(~ trajectory_type_l, nrow = 1) +
  theme_bw() +
  coord_equal() +
  labs(x = "Percentage errored", y = "Overall score") +
  scale_color_manual(values = set_names(c("black", "black", trajectory_types$colour), c("all", "overall", trajectory_types$id))) +
  theme(legend.position = "none", panel.grid = element_blank()) +
  scale_x_continuous(breaks = c(0, 1)) +
  scale_y_continuous(breaks = c(0, 1))
h

ggsave(figure_file("errormetric_comparison.svg"), h, width = 10, height = 5)
