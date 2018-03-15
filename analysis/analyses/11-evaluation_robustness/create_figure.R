library(tidyverse)
library(dynalysis)

experiment("11-evaluation_robustness")

robustness_overview <- cowplot::plot_grid(
  read_rds(figure_file("real_synthetic_comparison.rds")),
  read_rds(figure_file("metrics_comparison.rds")),
  read_rds(figure_file("performance_dataset_variability_combined.rds")),
  ncol = 1,
  labels = "auto",
  rel_heights = c(1, 2, 2)
)
ggsave(figure_file("robustness.svg"), robustness_overview, width = 16, height = 14)
