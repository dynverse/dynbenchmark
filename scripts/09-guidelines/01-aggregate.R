#' Aggregation of the different bnchmarking results for dynguidelines
#' One big methods tibble is created, which contains the information for each method in each row
#' This tibble is nested if a particular experiment generates multiple data points for a method
#' Otherwise, the result is given by using {experiment_id}_{metric_id}

library(dynbenchmark)
library(tidyverse)

experiment("09-guidelines")

# gathers scaling, methods, benchmark results
methods_aggr <- read_rds(result_file("results.rds", experiment_id = "08-summary"))

# temporary fix for stringlytyped scaling columns
methods_aggr <- methods_aggr %>%
  mutate_at(vars(matches("scaling_pred_(time|mem)_")), as.numeric)

# benchmarking metrics
benchmark_metrics <- dynbenchmark::metrics_evaluated %>% filter(type != "overall")
benchmark_results <- read_rds(result_file("benchmark_results_unnormalised.rds", experiment_id = "06-benchmark"))
benchmark <- benchmark_results %>%
  select(-prior_df) %>%
  select(method_id, dataset_id, dataset_trajectory_type, !!!benchmark_metrics$metric_id) %>%
  mutate_if(is.character, factor) %>%
  nest(-method_id, .key = "benchmark")

methods_aggr <- methods_aggr %>%
  left_join(benchmark, "method_id")

# benchmarking datasets
benchmark_datasets_info <- load_datasets(unique(benchmark_results$dataset_id)) %>%
  select(id, source, trajectory_type)

# metrics
benchmark_metrics <- dyneval::metrics %>%
  filter(metric_id %in% c("correlation", "him", "F1_branches", "featureimp_wcor"))
benchmark_metrics$description <- "todo"

withr::with_dir(
  "../dynguidelines",
  {
    usethis::use_data(
      methods_aggr,
      benchmark_datasets_info,
      benchmark_metrics,
      internal = TRUE,
      compress = "xz",
      overwrite = TRUE
    )
  }
)

