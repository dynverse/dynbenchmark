library(tidyverse)
library(dynbenchmark)

experiment("03-metric_characterisation/01-metric_conformity")

dataset_design <- read_rds(derived_file("dataset_design.rds"))
datasets <- read_rds(derived_file("datasets.rds"))

# load perturbations = dynwrap::ti_methods
source(scripts_file("helper-02-perturbations.R"))
methods_suite <- perturbation_methods %>% process_methods_design()

# load rules
source(scripts_file("helper-02-rules.R"))

dataset <- datasets$linear_10_1
model <- perturb_remove_cells(datasets$linear_10_1, 1)

write_rds(dataset, "~/dataset.rds")
write_rds(model, "~/model.rds")

calculate_metrics(dataset, model)


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
parameters <- map(methods_suite$id, function(method_id) {
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
}) %>% set_names(methods_suite$id)
parameters_suite <- process_parameters_design(methods_suite, parameters)

datasets_suite <- datasets %>% process_datasets_design()

design <- lst(
  datasets = datasets_suite,
  methods = methods_suite,
  crossing,
  parameters = parameters_suite,
  priors = NULL %>% process_priors_design(),
  num_repeats = 1
)


##  ............................................................................
##  Run the crossing                                                        ####
metrics <- c("correlation", "rf_mse", "rf_rsq", "rf_nmse", "lm_mse", "lm_rsq", "lm_nmse", "edge_flip")

# run evaluation on cluster
# benchmark_submit(design, metrics = metrics, qsub_params = list(memory = "2G", timeout = 3600))
# benchmark_fetch_results()
#
# results <- benchmark_bind_results()

# run evaluation locally
benchmark_submit_check(design, metrics)
results <- pbapply::pblapply(cl=8, seq_len(nrow(design$crossing)), benchmark_run_evaluation, subdesign = design, metric = metrics, verbose = TRUE) %>%
  bind_rows() %>%
  mutate(error_status = "no_error")

if (any(results$error_message != "")) stop("Errors: ", results %>% filter(error_message != "") %>% pull(method_id) %>% unique() %>% glue::glue_collapse(", "))

# extract scores from successful results
scores <- results %>%
  filter(error_status == "no_error") %>%
  gather("metric_id", "score", intersect(metrics, colnames(results))) %>%
  select(method_id, dataset_id, param_id, metric_id, score)

models <- results %>%
  select(method_id, dataset_id, param_id, model)

write_rds(scores, derived_file("scores.rds"))
write_rds(models, derived_file("models.rds"))
##
#
# split(scores, scores$metric_id) %>% map(function(scores) {
#   scores %>%
#     ggplot(aes(dataset_id, paste0(method_id, "_", param_id))) + geom_raster(aes(fill = score)) + ggtitle(scores$metric_id[[1]])
# }) %>% patchwork::wrap_plots()
