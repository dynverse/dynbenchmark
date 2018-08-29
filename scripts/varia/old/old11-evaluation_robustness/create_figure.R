library(tidyverse)
library(dynbenchmark)

experiment("11-evaluation_robustness")

robustness_overview <- cowplot::plot_grid(
  read_rds(result_file("real_synthetic_comparison.rds")),
  # read_rds(result_file("metrics_comparison.rds")),
  read_rds(result_file("performance_dataset_variability_combined.rds")),
  read_rds(result_file("dataset_sample_max_datasets_overview.rds")),
  ncol = 1,
  labels = "auto",
  rel_heights = c(1,  2, 1.5)
)
robustness_overview
ggsave(result_file("robustness.svg"), robustness_overview, width = 16, height = 12)
