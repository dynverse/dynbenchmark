#' Aggregation of the different bnchmarking results for user guidelines
#' One big methods tibble is created, which contains the information for each method in each row
#' This tibble is nested if a particular experiment generates multiple data points for a method
#' Otherwise, the result is given by using {experiment_id}_{metric_id}

library(dynbenchmark)
library(tidyverse)

experiment("09-guidelines")

# method information
methods <- read_rds(result_file("methods.rds", experiment_id = "03-methods"))
methods_aggr <- methods %>% rename(method_id = id)

# qc information
tool_qc_category_scores <- read_rds(result_file("tool_qc_category_scores.rds", experiment_id = "03-methods")) %>%
  nest(-tool_id, .key = "qc_category_scores")
tool_qc_application_scores <- read_rds(result_file("tool_qc_application_scores.rds", experiment_id = "03-methods")) %>%
  nest(-tool_id, .key = "qc_application_scores")
tool_qc <- read_rds(result_file("tool_qc.rds", experiment_id = "03-methods")) %>%
  nest(-tool_id, .key = "qc")

methods_aggr <- methods_aggr %>%
  left_join(tool_qc_category_scores, "tool_id") %>%
  left_join(tool_qc_application_scores, "tool_id")

# scaling
scaling_results <- read_rds(result_file("scaling.rds", experiment_id = "05-scaling"))
create_scaling_predictor <- function(model) {
  function(n_cells, n_features) {
    predicted <- VGAM::predict.rrvglm(model, list(lnrow = log(n_cells), lncol = log(n_features)))
    list(
      mean = exp(predicted[1, 1]),
      sd = exp(exp(predicted[1, 2]))
    )
  }
}
scaling_models <- scaling_results$models %>%
  select(-pct_errored) %>%
  mutate(
    model_time = model_time %>% map(strip_vglm) %>% map(create_scaling_predictor),
    model_mem = model_mem %>% map(strip_vglm) %>% map(create_scaling_predictor)
  ) %>%
  rename_at(vars(-one_of("method_id")), ~paste0("scaling_", .))
scaling_models$scaling_model_mem[[20]](100, 200)

methods_aggr <- methods_aggr %>%
  left_join(scaling_models, "method_id")

# benchmarking
benchmark_metrics <- dynbenchmark::metrics_evaluated %>% filter(type != "overall")
benchmark_results <- read_rds(result_file("benchmark_results_unnormalised.rds", experiment_id = "06-benchmark"))
benchmark <- benchmark_results$raw_data %>%
  select(-prior_df) %>%
  select(method_id, dataset_id, dataset_trajectory_type, !!!benchmark_metrics$metric_id) %>%
  mutate_if(is.character, factor) %>%
  nest(-method_id, .key = "benchmark")

methods_aggr <- methods_aggr %>%
  left_join(benchmark, "method_id")

# benchmarking datasets
benchmark_datasets_info <- load_datasets(unique(benchmark_results$raw_data$dataset_id)) %>%
  select(id, source, trajectory_type)

# overall scores
results <- read_rds(result_file("results.rds", experiment_id = "08-summary"))
overall_results <- results$results %>%
  filter(experiment != "scaling") %>%  # scaling experiment was already included
  mutate(metric_id = glue::glue("{experiment}_{metric}")) %>%
  select(method_id, value, metric_id) %>%
  spread(metric_id, value) %>%
  mutate(method_id = as.character(method_id))

methods_aggr <- methods_aggr %>%
  left_join(overall_results, "method_id")

# aggregate
methods_aggr %>% select(-benchmark, -parameters, -authors, -qc_category_scores)
methods_aggr %>% pryr::object_size()

devtools::use_data(methods_aggr, benchmark_datasets_info, internal = TRUE, pkg = "../dynguidelines")
# save(methods_aggr, benchmark_datasets_info, file = "../dynguidelines/R/sysdata.rda", compress = "xz")
