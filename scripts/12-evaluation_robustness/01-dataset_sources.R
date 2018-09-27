library(tidyverse)
library(dynbenchmark)

experiment("12-evaluation_robustness")

benchmark <- read_rds(result_file("benchmark_results_normalised.rds", "07-benchmark"))

overall_dataset_source_scores <- benchmark$data_aggregations %>%
  filter(dataset_trajectory_type == "overall") %>%
  select(method_id, dataset_source, overall) %>%
  spread(dataset_source, overall)

gold_dataset_source_scores <- overall_dataset_source_scores %>% gather("dataset_source", "dataset_source_score", -method_id, -real)

correlation_dataset_source_scores <- gold_dataset_source_scores %>%
  group_by(dataset_source) %>%
  summarise(cor = cor(real, dataset_source_score))

plot_dataset_source_correlation <- gold_dataset_source_scores %>%
  ggplot(aes(dataset_source_score, real)) +
    geom_abline(intercept = 0, slope = 1) +
    geom_point() +
    geom_text(aes(label = sprintf("%0.2f", cor)), x = 0.1, y = 0.9, data = correlation_dataset_source_scores) +
    facet_grid(.~dataset_source) +
    scale_x_continuous(limits = c(0, 1)) +
    scale_y_continuous(limits = c(0, 1)) +
    labs(x = "Overall performance on datasets from source", y = "Overall performance on real gold datasets") +
    theme_pub()

write_rds(plot_dataset_source_correlation, result_file("dataset_source_correlation"))
