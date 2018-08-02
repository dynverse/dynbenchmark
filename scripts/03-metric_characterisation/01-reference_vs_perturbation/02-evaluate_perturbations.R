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

rules <- lst(
  merge_bifurcation
)

# create the actual crossing, and use it to construct the design
crossing <- map_df(rules, function(rule) {
  crossing <- rule$crossing

  missing_cols <- setdiff(c("prior_id", "param_id", "repeat_ix"), colnames(crossing))
  crossing[, missing_cols] <- rep(c(prior_id = "none", param_id = "default", repeat_ix = 1)[missing_cols], each = nrow(crossing))
  crossing
})
crossing <- crossing[!duplicated(crossing),]

design <- lst(
  datasets = datasets_suite,
  methods = methods_suite,
  crossing,
  parameters = process_parameters_design(methods = methods, parameters = NULL),
  priors = NULL %>% process_priors_design(),
  num_repeats = 1
)

# define metrics
metrics <- dynbenchmark::metrics$metric_id

benchmark_submit(design, metrics = metrics)
benchmark_fetch_results()

results <- benchmark_bind_results()

scores <- results %>%
  filter(error_status == "no_error") %>%
  gather("metric_id", "score", intersect(metrics, colnames(results))) %>%
  select(method_id, dataset_id, param_id, metric_id, score)



split(scores, scores$metric_id) %>% map(function(scores) {
  scores %>%
    ggplot(aes(dataset_id, method_id)) + geom_raster(aes(fill = score)) + ggtitle(scores$metric_id[[1]])
}) %>% patchwork::wrap_plots()
