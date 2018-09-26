library(dynbenchmark)
library(tidyverse)

experiment("08-summary")

#####################################################
#                  GET METHODS INFO                 #
#####################################################

spread_trajtypes <- function(method_info) {
  trajtypes <-
    map_df(dynwrap::trajectory_types$id, function(trajtyp) {
      data_frame(id = method_info$id, trajectory_type = paste0("trajtyp_", trajtyp), value = map_lgl(method_info$trajectory_types, ~ trajtyp %in% .))
    }) %>%
    spread(trajectory_type, value)

  left_join(method_info, trajtypes, by = "id")
}

method_info <-
  read_rds(result_file("methods.rds", experiment_id = "03-methods")) %>%
  mutate(
    priors_required = map_chr(input, ~ .$required %>% setdiff(c("expression", "counts")) %>% paste0(collapse = ",")),
    priors_optional = map_chr(input, ~ .$optional %>% paste0(collapse = ",")),
    any_priors_required = priors_required != "",
    any_priors_optional = priors_optional != ""
  ) %>%
  spread_trajtypes() %>%
  rename_all(function(x) paste0("method_", x)) %>%
  rename(tool_id = method_tool_id) %>%
  select_if(is.atomic) %>%
  select_if(function(x) !all(is.na(x))) %>%
  filter(!method_id %in% c("error", "identity", "random", "shuffle"))

rm(spread_trajtypes) # writing tidy code

#####################################################
#                  READ QC RESULTS                  #
#####################################################
tool_qc_scores <- read_rds(result_file("tool_qc_scores.rds", experiment_id = "03-methods"))
tool_qc_category_scores <- read_rds(result_file("tool_qc_category_scores.rds", experiment_id = "03-methods"))
tool_qc_application_scores <- read_rds(result_file("tool_qc_application_scores.rds", experiment_id = "03-methods"))

qc_results <-
  method_info %>%
  select(method_id, tool_id) %>%
  left_join(
    tool_qc_scores %>% select(tool_id, qc_overall_overall = qc_score),
    by = "tool_id"
  ) %>%
  left_join(
    tool_qc_category_scores %>% mutate(category = paste0("qc_cat_", category)) %>% spread(category, qc_score),
    by = "tool_id"
  ) %>%
  left_join(
    tool_qc_application_scores %>% mutate(application = paste0("qc_app_", application)) %>% spread(application, score),
    by = "tool_id"
  ) %>%
  select(-tool_id)

rm(tool_qc_scores, tool_qc_category_scores, tool_qc_application_scores) # is important in large scripts

#####################################################
#                READ SCALING RESULTS               #
#####################################################
scaling_results <-
  read_rds(result_file("scaling_scores.rds", experiment_id = "05-scaling"))$scaling_scores %>%
  mutate(metric = paste0("scaling_preds_", metric)) %>%
  spread(metric, score)

#####################################################
#             READ BENCHMARKING RESULTS             #
#####################################################
benchmark_results_input <- read_rds(result_file("benchmark_results_input.rds", experiment_id = "07-benchmark"))
benchmark_results_normalised <- read_rds(result_file("benchmark_results_normalised.rds", experiment_id = "07-benchmark"))

execution_metrics <- c("pct_errored", "pct_execution_error", "pct_memory_limit", "pct_method_error_all", "pct_method_error_stoch", "pct_time_limit")
bench_metrics <- paste0("norm_", benchmark_results_input$metrics)
all_metrics <- c(bench_metrics, "overall", execution_metrics)

data_aggs <-
  benchmark_results_normalised$data_aggregations %>%
  select(method_id, param_id, dataset_trajectory_type, dataset_source, !!all_metrics)

bench_overall <-
  data_aggs %>%
  filter(dataset_trajectory_type == "overall", dataset_source == "mean") %>%
  select(method_id, param_id, !!set_names(all_metrics, paste0("benchmark_overall_", all_metrics)))

bench_trajtypes <-
  data_aggs %>%
  filter(dataset_trajectory_type != "overall", dataset_source == "mean") %>%
  transmute(method_id, metric = paste0("benchmark_tt_", dataset_trajectory_type), score = overall) %>%
  spread(metric, score)

bench_sources <-
  data_aggs %>%
  filter(dataset_trajectory_type == "overall", dataset_source != "mean") %>%
  transmute(method_id, metric = paste0("benchmark_source_", gsub("/", "_", dataset_source)), score = overall) %>%
  spread(metric, score)

benchmark_results <-
  left_join(
    bench_overall,
    bench_trajtypes,
    by = "method_id"
  ) %>%
  left_join(
    bench_sources,
    by = "method_id"
  )

rm(execution_metrics, bench_metrics, all_metrics, data_aggs, bench_overall, bench_trajtypes, bench_sources, benchmark_results_input, benchmark_results_normalised) # more than this haiku

#####################################################
#                  COMBINE RESULTS                  #
#####################################################
results <-
  method_info %>%
  left_join(qc_results, by = "method_id") %>%
  left_join(benchmark_results, by = "method_id") %>%
  left_join(scaling_results, by = "method_id")

rm(qc_results, benchmark_results, scaling_results)

#####################################################
#              DETERMINE FINAL RANKING              #
#####################################################
metric_weights <-
  c(
    benchmark_overall_overall = 2,
    qc_overall_overall = 1,
    scaling_preds_overall = 1
  )

results$summary_overall_overall <-
  results %>%
  select(!!names(metric_weights)) %>%
  mutate_all(function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x)) %>%
  dyneval::calculate_geometric_mean(weights = metric_weights)


#####################################################
#                    WRITE OUTPUT                   #
#####################################################
write_rds(results, result_file("results.rds"), compress = "xz")

