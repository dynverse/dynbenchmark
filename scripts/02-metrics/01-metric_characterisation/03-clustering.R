#'  Characterisation of the `r dynbenchmark::label_metric("F1_branches")` and `r dynbenchmark::label_metric("F1_milestones")`

library(dynbenchmark)
library(tidyverse)
library(patchwork)

experiment("02-metrics/01-metric_characterisation")

set.seed(9)
dataset <- dyntoy::generate_dataset(model = dyntoy::model_binary_tree(num_branchpoints = 2)) %>% simplify_trajectory()

plot_clustering_scores_overview <- wrap_plots(
  plot_graph(dataset, label_milestones = TRUE) + ggtitle("Reference"),
  plot_graph(dataset, grouping = group_onto_nearest_milestones(dataset), plot_milestones = TRUE) + ggtitle("Cells mapped to milestones"),
  plot_graph(dataset, grouping = group_onto_trajectory_edges(dataset)) + ggtitle("Cells mapped to branches") + guides(color = guide_legend(ncol = 3))
)

plot_clustering_scores_overview

write_rds(plot_clustering_scores_overview, result_file("clustering_scores_overview.rds"))

ggsave(result_file("clustering_scores_overview.pdf"), width = 12, height = 5)
