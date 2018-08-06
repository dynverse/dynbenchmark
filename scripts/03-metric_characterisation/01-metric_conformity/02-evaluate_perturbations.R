library(tidyverse)
library(dynbenchmark)

experiment("03-metric_characterisation/01-metric_conformity")

dataset_design <- read_rds(derived_file("dataset_design.rds"))
datasets <- read_rds(derived_file("datasets.rds"))

# load perturbations = dynwrap::ti_methods
source(scripts_file("helper-perturbations.R"))

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
parameters <- map(perturbation_methods %>% map_chr("id"), function(method_id) {
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
}) %>% set_names(perturbation_methods %>% map_chr("id"))

design <- benchmark_generate_design(
  datasets = datasets,
  methods = perturbation_methods,
  parameters = parameters,
  num_repeats = 1,
  crossing = crossing %>% filter(method_id == "switch_cells")
)


##  ............................................................................
##  Run the crossing                                                        ####
metrics <- c("correlation", "rf_mse", "rf_rsq", "rf_nmse", "lm_mse", "lm_rsq", "lm_nmse", "edge_flip")

# run evaluation on cluster
benchmark_submit(design, metrics = metrics, qsub_params = list(memory = "2G", timeout = 3600), qsub_grouping = "{method_id}")

benchmark_fetch_results()

results <- benchmark_bind_results(load_models = TRUE)

# run evaluation locally
# benchmark_submit_check(design, metrics)
# results <- pbapply::pblapply(cl=8, seq_len(nrow(design$crossing)), benchmark_run_evaluation, subdesign = design, metric = metrics, verbose = TRUE) %>%
#   bind_rows() %>%
#   mutate(error_status = ifelse(error_message == "", "no_error", "method_error"))

###
if (any(results$error_status != "no_error")) stop("Errors: ", results %>% filter(error_status != "no_error") %>% pull(method_id) %>% unique() %>% glue::glue_collapse(", "))

results %>% filter(error_status != "no_error") %>% select(method_id, error_message) %>% distinct()

# extract scores from successful results
scores <- results %>%
  filter(error_status == "no_error") %>%
  gather("metric_id", "score", intersect(metrics, colnames(results))) %>%
  select(method_id, dataset_id, param_id, metric_id, score)

if (any(is.na(scores$score))) stop("Some scores are NA!", score %>% filter(is.na(score)) %>% pull(metric_id) %>% unique())

models <- results %>%
  select(method_id, dataset_id, param_id, model)

write_rds(scores, derived_file("scores.rds"))
write_rds(models, derived_file("models.rds"))

#
# split(scores, scores$metric_id) %>% map(function(scores) {
#   scores %>%
#     ggplot(aes(dataset_id, paste0(method_id, "_", param_id))) + geom_raster(aes(fill = score)) + ggtitle(scores$metric_id[[1]])
# }) %>% patchwork::wrap_plots()
