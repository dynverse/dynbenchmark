#' Combine the different figures of this experiment into one

library(tidyverse)
library(dynbenchmark)
library(patchwork)

experiment("10-benchmark_interpretation")

plot_variability_dataset_source <- read_rds(derived_file("variability_dataset_source.rds"))
plot_variability_trajectory_type <- read_rds(derived_file("variability_trajectory_type.rds"))
plot_dataset_source_correlation <- read_rds(derived_file("dataset_source_correlation.rds"))
plot_topology_complexity_examples <- read_rds(derived_file("topology_complexity_examples.rds"))

plot_benchmark_interpretation <- wrap_plots(
  plot_variability_dataset_source %>% patchwork::wrap_elements(),
  plot_dataset_source_correlation %>% patchwork::wrap_elements(),
  plot_variability_trajectory_type %>% patchwork::wrap_elements(),
  plot_topology_complexity_examples %>% patchwork::wrap_elements(),
  ncol = 1,
  heights = c(1, 0.7, 1, 1)
) + plot_annotation(tag_levels = "a")

ggsave(result_file("benchmark_interpretation.pdf"), plot_benchmark_interpretation, width = 14, height = 18)
