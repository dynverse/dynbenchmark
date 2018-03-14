library(tidyverse)
library(dynalysis)
library(cowplot)

experiment("11-evaluation_robustness")

robustness_overview <- plot_grid(
  read_rds(figure_file("real_synthetic_comparison.rds")),
  read_rds(figure_file("performance_dataset_variability_combined.rds")),
  ncol=1,
  labels = "auto",
  rel_heights = c(1, 3)
)
save_plot(figure_file("robustness.svg"), robustness_overview)
