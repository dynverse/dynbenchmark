#' Compare the complexity of the topologies predicted by the methods

library(dynbenchmark)
library(tidyverse)

experiment("10-benchmark_interpretation")

statistics <- read_rds(derived_file("statistics.rds"))

##  ............................................................................
##  Overall ordering in complexity                                          ####

# determine order based on average
method_order <- statistics %>% arrange(complexity_difference_mean) %>% pull(method_id) %>% unique()

complexity_difference_range <- c(-25L, 25L)
bandwidth <- 1

plot_topology_complexity_difference <- statistics %>%
  mutate(method_id = factor(method_id, method_order)) %>%
  mutate(complexity_difference = scales::squish(complexity_difference, complexity_difference_range)) %>%
  ggplot(aes(complexity_difference, method_id)) +
  ggridges::geom_density_ridges2(aes(fill = complexity_difference_mean), bandwidth = bandwidth) +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed") +
  theme_pub() +
  scale_x_continuous(expand = c(0, 0), limits = ) +
  scale_y_discrete(NULL, labels = label_method) +
  scale_fill_distiller(label_long("average_complexity_difference"), palette = "BrBG", limits = complexity_difference_range, oob = scales::squish) +
  theme(legend.position = "top", legend.justification = "center")

plot_topology_complexity_difference

ggsave(result_file("topology_complexity_difference.pdf"), plot_topology_complexity_difference, width = 6, height = 10)

##  ............................................................................
##  Trajectory type specific complexity                                     ####

method_ids <- c("paga", "slingshot", "paga_tree", "monocle_ddrtree", "pcreode")

# add a row "all trajectory types"
statistics_complexity <- bind_rows(
  statistics,
  statistics %>% mutate(trajectory_type_dataset = "all_trajectory_types")
)

# some parameters of the plot
bw <- 2

alpha <- 0.8
trajectory_type_colors <- c(set_names(dynwrap::trajectory_types$colour, dynwrap::trajectory_types$id), "all_trajectory_types" = "#333333")
arrow_y <- length(trajectory_type_colors) + 1

# determine range of complexity differences
complexity_difference_range <- c(-25L, 25L)
statistics_complexity$complexity_difference_squished <- scales::squish(statistics_complexity$complexity_difference, complexity_difference_range)
complexity_difference_limits <- complexity_difference_range

arrow_annot_data <- tibble(
  method_id = factor(method_ids[[1]], method_ids),
  x = complexity_difference_limits,
  text = c("Prediction too\nsimple", "Prediction too\ncomplex"),
  hjust = c(0, 1)
)

square_trans <- scales::trans_new("square", function(x) x^2, function(x) sqrt(x))

plot_topology_complexity_examples <- statistics_complexity %>%
  mutate(trajectory_type_dataset = factor(trajectory_type_dataset, names(trajectory_type_colors))) %>%
  filter(method_id %in% method_ids) %>%
  mutate(method_id = factor(method_id, method_ids)) %>%
  ggplot(aes(complexity_difference_squished, trajectory_type_dataset)) +
  ggridges::geom_density_ridges2(
    aes(fill = trajectory_type_dataset),
    bandwidth = bw,
    alpha = alpha,
    from = complexity_difference_range[[1]],
    to = complexity_difference_range[[2]]
  ) +
  # geom_point() +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed") +
  geom_text(aes(x = x, y = arrow_y, hjust = hjust, label = text), colour = "#333333", vjust = 1, lineheight = 0.8, size = 3.2, data = arrow_annot_data %>% mutate(arrow_y = arrow_y)) +
  facet_grid(.~method_id, labeller = label_facet(label_method)) +
  scale_fill_manual(values = trajectory_type_colors, labels = label_long, guide = "none") +
  scale_y_discrete(label_long("Reference trajectory type"), expand = c(0,0), labels = label_long) +
  scale_x_continuous(label_long("Difference in topology size (= # milestones + # edges)\nbetween prediction and reference"), expand = c(0, 0), limits = complexity_difference_limits) +
  theme_pub()

plot_topology_complexity_examples

ggsave(result_file("topology_complexity_examples.pdf"), plot_topology_complexity_examples, width = 14, height = 5)

write_rds(plot_topology_complexity_examples, derived_file("topology_complexity_examples.rds"))
