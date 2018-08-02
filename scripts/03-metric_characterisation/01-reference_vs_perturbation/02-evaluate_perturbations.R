library(tidyverse)
library(dynbenchmark)

experiment("03-metric_characterisation/01-reference_vs_perturbation")

dataset_design <- read_rds(derived_file("dataset_design.rds"))
datasets_suite <- read_rds(derived_file("datasets_suite.rds"))

# load perturbations = dynwrap::ti_methods
source(scripts_file("helper-02-perturbations.R"))
methods_suite <- perturbation_methods %>% process_methods_design()

# load rules
source(scripts_file("helper-02-rules.R"))

# create the actual crossing, and use it to construct the design
crossing <- mapdf(rules, function(rule) {
  crossing <- rule$crossing

  missing_cols <- setdiff(c("prior_id", "param_id", "repeat_ix"), colnames(crossing))
  crossing[, missing_cols] <- rep(c(prior_id = "none", param_id = "default", repeat_ix = 1)[missing_cols], each = nrow(crossing))
  crossing
}) %>% bind_rows()
crossing <- crossing[!duplicated(crossing),]

# join all parameters from the different rules and remove duplicates
parameters <- map(methods_suite$id, function(method_id) {
  parameters <- rules %>% mapdf(
    function(rule) {
      if(method_id %in% names(rule$parameters)) {
        rule$parameters[[method_id]]
      } else {
        tibble(id = character())
      }
    }) %>% bind_rows()
  parameters
}) %>% set_names(methods_suite$id)
parameters_suite <- process_parameters_design(methods_suite, parameters)

design <- lst(
  datasets = datasets_suite,
  methods = methods_suite,
  crossing,
  parameters = parameters_suite,
  priors = NULL %>% process_priors_design(),
  num_repeats = 1
)

# define metrics
metrics <- c("correlation", "rf_mse", "rf_rsq", "rf_nmse", "lm_mse", "lm_rsq", "lm_nmse", "edge_flip")

# run evaluation on cluster
# benchmark_submit(design, metrics = metrics, qsub_params = list(memory = "2G", timeout = 3600))
# benchmark_fetch_results()
#
# results <- benchmark_bind_results()

# run evaluation locally
benchmark_submit_check(design, metrics)
results <- map_df(seq_len(nrow(design$crossing)), benchmark_run_evaluation, subdesign = design, metric = metrics, verbose = TRUE) %>%
  mutate(error_status = "no_error")

# extract scores from successful results
scores <- results %>%
  filter(error_status == "no_error") %>%
  gather("metric_id", "score", intersect(metrics, colnames(results))) %>%
  select(method_id, dataset_id, param_id, metric_id, score, model)

write_rds(scores, derived_file("scores.rds"))
##

split(scores, scores$metric_id) %>% map(function(scores) {
  scores %>%
    ggplot(aes(dataset_id, paste0(method_id, "_", param_id))) + geom_raster(aes(fill = score)) + ggtitle(scores$metric_id[[1]])
}) %>% patchwork::wrap_plots()
