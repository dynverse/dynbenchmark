library(dynbenchmark)
library(tidyverse)

experiment("07-benchmark")

# collect method ids to evaluate
method_ids <- dynmethods::methods$id
methods <-
  dynwrap::get_ti_methods(method_ids, evaluate = FALSE) %>%
  mapdf(function(m) {
    l <- m$method_func()
    l$fun <- m$method_func
    l$type <- "function"
    l
  }) %>%
  list_as_tibble() %>%
  select(id, type, fun, everything())

# combine default params and optimised params... if we had some!
parameters <- lapply(method_ids, function(mn) {
  default <- tibble(id = "default")
  # best <- ... %>% mutate(id = "optimised")
  # bind_rows(default, best)
  defaults
}) %>% set_names(method_ids)

# configure metrics
metrics <- c("correlation", "rf_mse", "edge_flip", "featureimp_cor")

# configure qsub params
qsub_params <- function(method_id, param_id) {
  prm <- lst(timeout = 6 * 60 * 60, memory = "8G")
  if (method_id %in% c("ctgibbs", "scimitar", "ouijaflow", "ouija", "pseudogp")) {
    prm$memory <- "32G"
  }
  prm
}

# create design
design <- benchmark_generate_design(
  datasets = list_datasets()$id,
  methods = method_ids,
  parameters = parameters,
  num_repeats = 4
)

# save configuration
write_rds(design, derived_file("design.rds"))

# submit job
benchmark_submit(
  design = design,
  qsub_grouping = "{method_id}/{param_id}",
  qsub_params = qsub_params,
  metrics = metrics,
  verbose = TRUE
)
