library(tidyverse)
library(dynbenchmark)
library(patchwork)

experiment("10-benchmark_interpretation")

plot_dataset_source_correlation <- read_rds(result_file("dataset_source_correlation.rds")) %>% wrap_elements()
plots_dataset_variability <- (read_rds(result_file("dataset_variability.rds"))$all + theme(legend.position = "none")) %>% wrap_elements()
plot_topology_complexity <- read_rds(result_file("topology_complexity.rds")) %>% wrap_elements()

plot_benchmark_interpretation <- wrap_plots(
  plots_dataset_variability,
  plot_dataset_source_correlation,
  plot_topology_complexity,
  ncol = 1,
  heights = c(1, 0.7, 1)
)

plot_benchmark_interpretation

write_rds(plot_benchmark_interpretation, result_file("benchmark_interpretation.rds"))
