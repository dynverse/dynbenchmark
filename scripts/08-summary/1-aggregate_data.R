#' Aggregation of the results
#' The main job is done by the dynbenchmark::benchmark_aggregate function

library(dynbenchmark)
library(tidyverse)

experiment("08-summary")

#####################################################
#                  GET METHODS INFO                 #
#####################################################
method_info <-
  load_methods() %>%
  mutate(
    required_priors_str = map_chr(wrapper_input_required, ~ setdiff(., c("expression", "counts")) %>% paste0(collapse = ",")),
    optional_priors_str = map_chr(wrapper_input_optional, ~ paste0(., collapse = ","))
  ) %>%
  rename(tool_id = method_tool_id) %>%
  select_if(function(x) !all(is.na(x)))

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

rm(tool_qc_scores, tool_qc_category_scores, tool_qc_application_scores) # writing tidy code

#####################################################
#                READ SCALING RESULTS               #
#####################################################
scaling_scores <-
  read_rds(result_file("scaling_scores.rds", experiment_id = "05-scaling"))$scaling_scores %>%
  mutate(overall = (scoretime + scoremem) / 2) %>%
  gather(column, score, scoretime, scoremem, overall) %>%
  filter(column != "overall" | metric == "overall") %>%
  mutate(metric = paste0("scaling_pred_", column, "_", metric)) %>%
  select(-column) %>%
  spread(metric, score)

scaling_preds <-
  read_rds(result_file("scaling_scores.rds", experiment_id = "05-scaling"))$scaling_preds %>%
  gather(column, value, time, timestr, mem, memstr) %>%
  mutate(metric = paste0("scaling_pred_", column, "_", labnrow, "_", labncol)) %>%
  select(method_id, metric, value) %>%
  spread(metric, value)

scaling_models <-
  read_rds(result_file("scaling.rds", experiment_id = "05-scaling"))$models %>%
  rename_at(., setdiff(colnames(.), "method_id"), function(x) paste0("scaling_models_", x))

#####################################################
#             READ BENCHMARKING RESULTS             #
#####################################################
benchmark_results_input <- read_rds(result_file("benchmark_results_input.rds", experiment_id = "06-benchmark"))
benchmark_results_normalised <- read_rds(result_file("benchmark_results_normalised.rds", experiment_id = "06-benchmark"))

bench_predcors <-
  benchmark_results_normalised$data %>%
  select(method_id, param_id, time_lpred, time_pred, mem_lpred, mem_pred, time, ltime, mem, lmem) %>%
  group_by(method_id, param_id) %>%
  summarise(
    benchmark_overall_time_predcor = nacor(time_lpred, ltime),
    benchmark_overall_mem_predcor = nacor(mem_lpred, lmem)
  ) %>%
  ungroup()

execution_metrics <- c("pct_errored", "pct_execution_error", "pct_memory_limit", "pct_method_error_all", "pct_method_error_stoch", "pct_time_limit")
bench_metrics <- paste0("norm_", benchmark_results_input$metrics %>% setdiff(c("edge_flip", "featureimp_cor")))
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
  transmute(method_id, param_id, metric = paste0("benchmark_tt_", dataset_trajectory_type), score = overall) %>%
  spread(metric, score)

bench_sources <-
  data_aggs %>%
  filter(dataset_trajectory_type == "overall", dataset_source != "mean") %>%
  transmute(method_id, param_id, metric = paste0("benchmark_source_", gsub("/", "_", dataset_source)), score = overall) %>%
  spread(metric, score)

bench <- bench_overall %>%
  left_join(bench_trajtypes, by = c("method_id", "param_id")) %>%
  left_join(bench_sources, by = c("method_id", "param_id")) %>%
  left_join(bench_predcors, by = c("method_id", "param_id"))

rm(
  execution_metrics, bench_metrics, all_metrics, data_aggs, benchmark_results_input, benchmark_results_normalised,
  bench_overall, bench_trajtypes, bench_sources, bench_predcors
) # is important in large scripts

#####################################################
#               READ STABILITY RESULTS              #
#####################################################
stability <- read_rds(result_file("stability_results.rds", experiment_id = "07-stability"))$summ %>%
  rename_at(., setdiff(colnames(.), "method_id"), ~ paste0("stability_", .)) %>%
  rename(stability_overall_overall = stability_geom_mean)

#####################################################
#                  COMBINE RESULTS                  #
#####################################################

results <- Reduce(
  function(a, b) left_join(a, b, by = "method_id"),
  list(method_info, qc_results, scaling_scores, scaling_preds, scaling_models, bench, stability)
)

rm(list = setdiff(ls(), "results")) # more than this haiku

#####################################################
#              DETERMINE FINAL RANKING              #
#####################################################
metric_weights <-
  c(
    benchmark_overall_overall = 2,
    qc_overall_overall = 1,
    scaling_pred_overall_overall = 1,
    stability_overall_overall = 1
  )

results$summary_overall_overall <-
  results %>%
  select(!!names(metric_weights)) %>%
  mutate_all(function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x)) %>%
  dynutils::calculate_geometric_mean(weights = metric_weights)


g <- GGally::ggpairs(results %>% select(summary_overall_overall, !!names(metric_weights))) + theme_bw()
ggsave(result_file("compare_metrics.pdf"), g, width = 10, height = 10)


#####################################################
#                    WRITE OUTPUT                   #
#####################################################
write_rds(results, result_file("results.rds"), compress = "xz")

