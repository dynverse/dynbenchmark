#' Perturbing the reference datasets (for example shuffling the cells) to check the effect of this perturbation on a metric

library(tidyverse)
library(dynbenchmark)

experiment("02-metrics/02-metric_conformity")

dataset_design <- read_rds(result_file("dataset_design.rds"))
perturbation_methods <- dynbenchmark:::perturbation_methods_design

# load rules
source(scripts_file("helper-rules.R"))

##  ............................................................................
##  Define which combinations of methods and datasets should be run         ####

# create the crossing first
crossing <- mapdf(rules, function(rule) {
  crossing <- rule$crossing

  missing_cols <- setdiff(c("prior_id", "param_id", "repeat_ix"), colnames(crossing))
  crossing[, missing_cols] <- rep(c(prior_id = "none", param_id = "default", repeat_ix = 1)[missing_cols], each = nrow(crossing))
  crossing
}) %>% bind_rows()
crossing <- crossing[!duplicated(crossing),]

# join all parameters from the different rules and remove duplicates
parameters <- map(perturbation_methods$id, function(method_id) {
  parameters <- rules %>% mapdf(
    function(rule) {
      if(method_id %in% names(rule$parameters)) {
        rule$parameters[[method_id]]
      } else {
        tibble(id = character())
      }
    }) %>% bind_rows()
  parameters <- parameters[!duplicated(parameters),]
  parameters
}) %>% set_names(perturbation_methods$id)

# get dataset functions
datasets <- dynbenchmark:::process_datasets_design(dataset_design %>% select(dataset_id, dataset) %>% deframe())

design <- benchmark_generate_design(
  datasets = datasets,
  methods = perturbation_methods,
  parameters = parameters,
  num_repeats = 1,
  crossing = crossing# %>% filter(method_id == "identity") %>% sample_n(3)
)


##  ............................................................................
##  Run the crossing                                                        ####
metric_ids <- metrics_characterised %>%
  filter(type != "overall") %>%
  pull(metric_id)

metrics <- as.list(metric_ids)

metric_ids <- ifelse(map_lgl(metrics, is.character), metrics, names(metrics)) %>% unlist() %>% unname()
# test first:
# out <- evaluate_ti_method(generate_dataset(), ti_angle(), parameters = list(), metrics = metrics)
# out$summary %>% as.data.frame

# run evaluation on cluster
benchmark_submit(
  design,
  metrics = metrics,
  qsub_params = list(memory = "10G", timeout = 3600),
  qsub_grouping = "{method_id}",
  output_models = FALSE
)

benchmark_fetch_results()

results <- benchmark_bind_results(load_models = FALSE)

# run evaluation locally
# benchmark_submit_check(design, metrics)
# results <- pbapply::pblapply(
#   cl=1,
#   design$crossing %>%
#     mutate(x = row_number()) %>%
#     filter(method_id == "shuffle_cells") %>%
#     sample_n(10) %>%
#     pull(x),
#   benchmark_run_evaluation,
#   subdesign = design,
#   metric = metrics,
#   verbose = TRUE
# ) %>%
#   bind_rows() %>%
#   mutate(error_status = ifelse(error_message == "", "no_error", "method_error"))

###
if (any(results$error_status != "no_error")) stop("Errors: ", results %>% filter(error_status != "no_error") %>% pull(method_id) %>% unique() %>% glue::glue_collapse(", "))

results %>% filter(error_status != "no_error") %>% select(method_id, error_message, stderr) %>% distinct() %>% pull(stderr)

results %>% group_by(method_id) %>% summarise(error_pct = mean(error_status != "no_error")) %>% arrange(error_pct)

# extract scores from successful results
scores <-
  results %>%
  filter(error_status == "no_error") %>%
  gather("metric_id", "score", intersect(metric_ids, colnames(results))) %>%
  select(method_id, dataset_id, param_id, metric_id, score)

if (any(is.na(scores$score))) stop("Some scores are NA!", score %>% filter(is.na(score)) %>% pull(metric_id) %>% unique())

write_rds(scores, derived_file("scores.rds"))

