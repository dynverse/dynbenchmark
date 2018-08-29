#' Start the method testing on the cluster

library(dynbenchmark)
library(tidyverse)

experiment("04-method_testing")

###################################################
###                   DESIGN                    ###
###################################################

methods <- dynwrap::get_ti_methods(dynmethods::repo_digests) %>%
  mutate(type = "fun")

design <- benchmark_generate_design(
  datasets = list(
    dyntoy::generate_dataset(id = "toy/linear", model = "linear", num_cells = 99, num_features = 101),
    dyntoy::generate_dataset(id = "toy/bifurcating", model = "bifurcating", num_cells = 99, num_features = 101),
    "real/developing-dendritic-cells_schlitzer",
    "real/fibroblast-reprogramming_treutlein"
  ),
  methods = methods %>% select(id, type, fun, repo_digests),
  parameters = list(
    fateid = tibble(id = "default", force = TRUE),
    stemnet = tibble(id = "default", force = TRUE),
    tscan = tibble(id = "default", modelNames = list(c("VVV", "EEE")))
  )
)

# also include dynmethods container examples
examples <- bind_rows(pbapply::pblapply(
  methods$id,
  cl = 8,
  function(method_id) {
    file <- paste0("../dynmethods/containers/", method_id, "/example.R")
    source(file)
    tibble(
      method_id = method_id,
      dataset = list(data),
      dataset_id = data$id,
      params = list(params),
      param_id = "example"
    )
  }
))
testthat::expect_equal(examples$dataset_id %>% str_replace_all("^specific_example/", ""), examples$method_id)

design$parameters <- bind_rows(
  design$parameters,
  examples %>% select(id = param_id, method_id, params)
)
design$crossing <- bind_rows(
  design$crossing,
  examples %>% mutate(prior_id = "none", repeat_ix = 1) %>% select(dataset_id, method_id, prior_id, repeat_ix, param_id)
)
design$datasets <- bind_rows(
  design$datasets,
  dynbenchmark:::process_datasets_design(examples$dataset)
)

write_rds(design, derived_file("design.rds"), compress = "xz")

###################################################
###                    SUBMIT                   ###
###################################################
design <- read_rds(derived_file("design.rds"))

# design$crossing <- design$crossing %>% filter(method_id == "angle")

benchmark_submit(
  design = design,
  qsub_grouping = "{method_id}",
  qsub_params = function(method_id) lst(timeout = 1200, memory = ifelse(method_id %in% c("ouija", "ouijaflow", "paga", "scimitar"), "32G", "10G")),
  metrics = c("correlation", "edge_flip", "rf_rsq", "featureimp_cor", "him")
)

