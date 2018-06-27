library(cowplot)
library(tidyverse)
library(dynbenchmark)

library(tidygraph)
library(ggraph)

experiment("8-compare_topology")

read_rds(derived_file("evaluation_algorithm.rds", "5-optimise_parameters/10-aggregations")) %>% list2env(.GlobalEnv)

label_facet_methods <- function(x) {tibble(method_short_name = methods$method_name[match(x$method_short_name, methods$method_short_name)])}

##  ............................................................................
##  Performance on traj type vs can handle traj type                        ####
method_trajtypes <- methods %>%
  gather(trajectory_type, can_handle_trajectory_type, !!trajectory_types$id[trajectory_types$directedness == "directed"])

scores <- c("harm_mean", "norm_edge_flip", "norm_correlation", "norm_rf_mse")

method_trajtypes_scores <- left_join(
  trajtype_scores,
  method_trajtypes,
  c("method_short_name", "trajectory_type")
) %>%
  mutate(trajectory_type = factor(trajectory_type, trajectory_types$id)) %>%
  select(method_short_name, trajectory_type, can_handle_trajectory_type, !!scores) %>%
  drop_na() %>%
  gather(score_id, score_value, !!scores)

get_violin_color <- function(trajectory_type, can_handle_trajectory_type) {
  ifelse(
    can_handle_trajectory_type,
    set_names(trajectory_types$colour, trajectory_types$id)[trajectory_type],
    set_names(trajectory_types$background_colour, trajectory_types$id)[trajectory_type]
  )
}

trajtype_handle_comparison <- method_trajtypes_scores %>%
  ggplot(aes(can_handle_trajectory_type, score_value)) +
  geom_violin(aes(fill = get_violin_color(trajectory_type, can_handle_trajectory_type))) +
  # geom_boxplot(aes(fill = get_violin_color(trajectory_type, can_handle_trajectory_type))) +
  geom_point(aes(fill = get_violin_color(trajectory_type, can_handle_trajectory_type)), pch = 21) +
  ggrepel::geom_label_repel(
    aes(
      can_handle_trajectory_type,
      score_value,
      label= label_short(set_names(methods$method_name, methods$method_short_name)[method_short_name], 10),
      color = get_violin_color(trajectory_type, can_handle_trajectory_type)
    ),
    method_trajtypes_scores %>%
      group_by(trajectory_type, can_handle_trajectory_type, score_id) %>%
      top_n(1, score_value+runif(n(), 0, 0.0000001)),
    nudge_y = 0.1,
    direction = "y",
    min.segment.length = 0
  ) +
  facet_grid(score_id~trajectory_type, labeller = label_facet(label_simple_trajectory_types))+
  scale_fill_identity() +
  scale_color_identity() +
  scale_y_continuous(label_long("score_on_datasets_with_trajectory_type"), limits = c(0, 1.2), breaks = c(0, 0.25, 0.5, 0.75, 1)) +
  scale_x_discrete(label_long("can_handle_trajectory_type"), labels = label_long) +
  annotate("segment", x = -Inf, xend = Inf, y = -Inf, yend = -Inf)+
  annotate("segment", x = -Inf, xend = -Inf, y = -Inf, yend = Inf) +
  theme(panel.spacing = unit(0, "cm"))
trajtype_handle_comparison
trajtype_handle_comparison %>% ggsave(figure_file("trajtype_handle_comparison.svg"), ., width = 15, height = 8)


##  ............................................................................
##  Balance between ordering and topology                                   ####
scores <- c("harm_mean", "norm_edge_flip", "norm_correlation", "norm_rf_mse")
# scores <- c("harm_mean", "edge_flip", "correlation", "rf_mse")
linear_split_scores <- indrep_scores %>%
  mutate(
    is_linear_restricted = ifelse(maximal_trajectory_type %in% c("directed_cycle", "directed_linear"), "linear_method", "non-linear_method"),
    trajectory_type_linear = ifelse(trajectory_type %in% c("directed_cycle", "directed_linear"), "linear_dataset", "more_complex_than_linear_dataset")
  ) %>%
  group_by(task_id, is_linear_restricted, trajectory_type_linear) %>%
  summarise_at(
    scores,
    max
  ) %>%
  ungroup() %>%
  gather(score_id, score_value, !!scores)

linear_split_tests <- linear_split_scores %>%
  group_by(trajectory_type_linear, score_id) %>%
  summarise(
    test = wilcox.test(
      score_value[is_linear_restricted == "linear_method"],
      score_value[is_linear_restricted != "linear_method"],
      conf.int = T,
      paired = T
    ) %>% list()
  ) %>% mutate(
    p_value = map_dbl(test, "p.value"),
    q_value = p.adjust(p_value, "BH"),
    estimate = map_dbl(test, "estimate"),
    effect = ifelse(estimate > 0, "↘", "↗")
  )
linear_split_tests

linear_split_comparison <- linear_split_scores %>%
  ggplot(aes(is_linear_restricted, score_value)) +
  geom_violin(aes(fill = is_linear_restricted)) +
  ggbeeswarm::geom_quasirandom(alpha = 0.8, size = 0.5) +
  geom_text(aes(label = paste0(effect, ": ", label_pvalue(q_value))), x = 1.5, y = 1.1, linear_split_tests, size = 5) +
  facet_grid(score_id~trajectory_type_linear, scales = "free_y", labeller = label_facet(function(x) label_short(x, 15))) +
  scale_y_continuous(label_long("maximal_score_on_dataset"), limits = c(NA, 1), expand = c(0, 0.2)) +
  scale_x_discrete(label_long("method_restriction"), label = label_short) +
  scale_fill_manual(values = set_names(trajectory_types$colour[match(c("directed_linear", "rooted_tree"), trajectory_types$id)], c("linear_method", "non-linear_method"))) +
  theme(legend.position = "none", strip.text.y = element_text(angle = 0))
linear_split_comparison
ggsave(figure_file("linear_split_comparison.svg"), linear_split_comparison, width = 6, height = 8)


##  ............................................................................
##  Edge flip distributions                                                 ####
bw = 0.05
edge_flip_distributions <- ind_scores %>%
  filter(method_short_name %in% method_order) %>%
  mutate(method_short_name = factor(method_short_name, rev(method_order))) %>%
  mutate(trajectory_type = factor(trajectory_type, trajectory_types$id)) %>%
  ggplot(aes(edge_flip, method_short_name)) +
  ggridges::geom_density_ridges2(
    aes(fill = trajectory_type),
    stat = "density_ridges",
    alpha = 0.8,
    bandwidth = bw,
    from = 0,
    to = 1
  ) +
  scale_fill_manual(values = set_names(trajectory_types$colour, trajectory_types$id)) +
  facet_grid(.~trajectory_type, labeller = label_facet(label_simple_trajectory_types)) +
  scale_y_discrete(label_long("method_short_name"), expand = c(0, 0), labels = set_names(methods$method_name, methods$method_short_name)) +
  scale_x_continuous(label_long("edge_flip"), expand = c(0, 0), breaks = c(0, 0.5, 1), label = round) +
  theme(legend.position = "none", axis.text.y = element_text(vjust = 0))
edge_flip_distributions


