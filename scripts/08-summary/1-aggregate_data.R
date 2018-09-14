library(dynbenchmark)
library(tidyverse)

experiment("08-summary")

# read methods info
method_info <-
  read_rds(result_file("methods.rds", experiment_id = "03-methods")) %>%
  rename_all(function(x) paste0("method_", x)) %>%
  rename(tool_id = method_tool_id) %>%
  select_if(is.atomic) %>%
  select_if(function(x) !all(is.na(x))) %>%
  filter(!method_id %in% c("error", "identity", "random", "shuffle"))

# read QC results
tool_qc_scores <- read_rds(result_file("tool_qc_scores.rds", experiment_id = "03-methods"))
tool_qc_scores <- read_rds(result_file("tool_qc_scores.rds", experiment_id = "03-methods"))
tool_qc_category_scores <- read_rds(result_file("tool_qc_category_scores.rds", experiment_id = "03-methods"))
tool_qc_application_scores <- read_rds(result_file("tool_qc_application_scores.rds", experiment_id = "03-methods"))

method_qc_overall_scores <-
  method_info %>%
  select(method_id, tool_id) %>%
  inner_join(tool_qc_scores, by = "tool_id") %>%
  mutate(experiment = "qc", category = "overall", metric = "overall") %>%
  select(method_id, experiment, category, metric, value = qc_score)

method_qc_category_scores <-
  method_info %>%
  select(method_id, tool_id) %>%
  inner_join(tool_qc_category_scores, by = "tool_id") %>%
  rename(metric = category) %>%
  mutate(experiment = "qc", category = "category") %>%
  select(method_id, experiment, category, metric, value = qc_score)

method_qc_application_scores <-
  method_info %>%
  select(method_id, tool_id) %>%
  inner_join(tool_qc_application_scores, by = "tool_id") %>%
  rename(metric = application) %>%
  mutate(experiment = "qc", category = "application") %>%
  select(method_id, experiment, category, metric, value = score)

qc_results <-
  bind_rows(
    method_qc_overall_scores,
    method_qc_category_scores,
    method_qc_application_scores
  )
rm(tool_qc_scores, tool_qc_category_scores, tool_qc_application_scores, method_qc_overall_scores, method_qc_category_scores, method_qc_application_scores)

# read scaling results
scaling_results <- read_rds(result_file("scaling.rds", experiment_id = "05-scaling"))

scaling_process <-
  scaling_results$models %>%
  select(-method_name, -model_time, -model_mem) %>%
  gather(metric, value, -method_id) %>%
  mutate(
    experiment = "scaling",
    category = case_when(metric == "pct_errored" ~ "errors", grepl("time", metric) ~ "time", grepl("mem", metric) ~ "memory")
  ) %>%
  group_by(category, metric) %>%
  mutate(value = percent_rank(-value)) %>%
  ungroup()

scaling_results <- scaling_process %>% {
  df <- .
  bind_rows(
    df,
    df %>%
      filter(metric %in% c("time_lpred", "mem_lpred")) %>%
      group_by(method_id, experiment) %>%
      summarise(value = dyneval::calculate_geometric_mean(value)) %>%
      ungroup() %>%
      mutate(category = "overall", metric = "overall")
  )
}

rm(scaling_process)


# read benchmarking results
benchmark_results_input <- read_rds(result_file("benchmark_results_input.rds", experiment_id = "07-benchmark"))
benchmark_results_normalised <- read_rds(result_file("benchmark_results_normalised.rds", experiment_id = "07-benchmark"))

data_aggs <-
  benchmark_results_normalised$data_aggregations %>%
  select(-n, -method_name) %>%
  gather(metric, value, -method_id:-dataset_source) %>%
  mutate(experiment = "benchmark")

execution_metrics <- c("pct_errored", "pct_execution_error", "pct_memory_limit", "pct_method_error_all", "pct_method_error_stoch", "pct_time_limit")
bench_metrics <- paste0("norm_", benchmark_results_input$metrics)

bench_overall <-
  data_aggs %>%
  filter(dataset_trajectory_type == "overall", dataset_source == "mean", metric %in% c(bench_metrics, "overall", execution_metrics)) %>%
  select(method_id, metric, value, experiment) %>%
  mutate(category = case_when(metric %in% execution_metrics ~ "execution", metric %in% bench_metrics ~ "metric", metric == "overall" ~ "overall"))

bench_trajtypes <-
  data_aggs %>%
  filter(dataset_trajectory_type != "overall", dataset_source == "mean", metric == "overall") %>%
  select(method_id, metric = dataset_trajectory_type, value, experiment) %>%
  mutate(category = "trajtypes")

bench_sources <-
  data_aggs %>%
  filter(dataset_trajectory_type == "overall", dataset_source != "mean", metric == "overall") %>%
  select(method_id, metric = dataset_source, value, experiment) %>%
  mutate(category = "sources")

benchmark_results <-
  bind_rows(
    bench_overall,
    bench_trajtypes,
    bench_sources
  )

rm(data_aggs, bench_overall, bench_trajtypes, bench_sources)

# combine different experiments
results <-
  bind_rows(qc_results, benchmark_results, scaling_results)

not_available_methods <- setdiff(unique(results$method_id), method_info$method_id)
warning("THESE METHODS DO NOT HAVE BENCHMARKING RESULTS: ", paste0(not_available_methods, collapse = ", "))

results <- results %>% filter(method_id %in% method_info$method_id)

metric_info <- results %>%
  group_by(experiment, category, metric) %>%
  summarise(min = min(value), max = max(value)) %>%
  ungroup() %>%
  mutate_if(is.numeric, function(x) round(x, 2))

metric_info %>% as.data.frame()

# make sure each method has a value for each metric
results <- full_join(
  results,
  crossing(method_info %>% select(method_id), metric_info %>% select(experiment, category, metric)),
  by = c("method_id", "experiment", "category", "metric")
) %>%
  group_by(experiment, metric, category) %>%
  mutate(placeholder = is.na(value), value = ifelse(placeholder, mean(value, na.rm = TRUE), value)) %>%
  ungroup()

rm(qc_results, benchmark_results, scaling_results)


## CALCULATE FINAL RANKING

metric_weights <-
  tribble(
    ~experiment, ~category, ~metric, ~weight,
    "benchmark", "overall", "overall", 4,
    "qc", "overall", "overall", 1,
    "scaling", "overall", "overall", 2
  )

results_final <-
  inner_join(
    results,
    metric_weights,
    by = c("experiment", "category", "metric")
  ) %>%
  group_by(method_id) %>%
  summarise(placeholder = FALSE, value = prod(value ^ weight) ^ (1 / sum(weight))) %>% #weighted geometric mean
  mutate(experiment = "summary", metric = "overall", category = "overall") %>%
  bind_rows(results)

metric_info <- metric_info %>% add_row(experiment = "summary", metric = "overall", category = "overall")

method_id_levels <-
  results_final %>%
  # filter(experiment == "summary") %>%
  filter(experiment == "benchmark", category == "overall") %>%
  arrange(desc(value)) %>%
  pull(method_id)

results <- results_final %>% mutate(method_id = factor(method_id, levels = method_id_levels))

rm(method_id_levels, results_final)

# # construct minis
# trajectory_types_mini <- tibble(
#   id = list.files(result_file("", "trajectory_types/mini")) %>% str_replace(".svg$", ""),
#   svg = id %>% map(~as.character(xml2::read_xml(result_file(paste0(., ".svg"), "trajectory_types/mini"))))
# )
#
# minis <- trajectory_types_mini %>% create_replacers()

# write output
write_rds(lst(method_info, results, metric_info), result_file("results.rds"), compress = "xz")

