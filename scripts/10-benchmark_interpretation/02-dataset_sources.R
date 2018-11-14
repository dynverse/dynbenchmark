#' Similarity of the results across dataset sources

library(tidyverse)
library(dynbenchmark)

experiment("10-benchmark_interpretation")

methods <- load_methods()
data <- read_rds(result_file("benchmark_results_normalised.rds", "06-benchmark"))$data %>% filter(method_id %in% methods$id)

# aggregate without errors
data_aggregations <- benchmark_aggregate(
  data = data %>% filter(error_status == "no_error")
)$data_aggregations

overall_dataset_source_scores <- data_aggregations %>%
  filter(dataset_trajectory_type == "overall", dataset_source != "mean") %>%
  select(method_id, dataset_source, overall) %>%
  spread(dataset_source, overall)

gold_dataset_source_scores <- overall_dataset_source_scores %>%
  gather("dataset_source", "dataset_source_score", -method_id, -`real/gold`)

correlation_dataset_source_scores <- gold_dataset_source_scores %>%
  group_by(dataset_source) %>%
  summarise(cor = nacor(`real/gold`, dataset_source_score)) %>%
  arrange(-cor) %>%
  mutate(dataset_source = fct_inorder(dataset_source))

plot_dataset_source_correlation <- gold_dataset_source_scores %>%
  mutate(dataset_source = factor(dataset_source, levels = levels(correlation_dataset_source_scores$dataset_source))) %>%
  ggplot(aes(dataset_source_score, `real/gold`)) +
    geom_abline(intercept = 0, slope = 1) +
    geom_point() +
    geom_label(aes(label = sprintf("corr = %0.2f", cor)), x = 0.1, y = 0.9, data = correlation_dataset_source_scores, vjust = 0, hjust = 0) +
    facet_grid(.~dataset_source) +
    scale_x_continuous(limits = c(0, 1)) +
    scale_y_continuous(limits = c(0, 1)) +
    labs(x = "Overall score on datasets from source", y = "Overall score on real/gold datasets") +
    theme_pub()

plot_dataset_source_correlation

write_rds(plot_dataset_source_correlation, derived_file("dataset_source_correlation.rds"))
