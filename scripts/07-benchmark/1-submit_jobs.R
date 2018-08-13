library(dynbenchmark)
library(tidyverse)

experiment("07-benchmark")

##########################################################
###############       DEFINE METHODS       ###############
##########################################################

scaling <- read_rds(result_file("scaling.rds", "05-scaling"))

# need to look into scaling results of these methods first
method_ids <- scaling$models %>%
  filter(!method_id %in% c("merlot", "raceid", "slice", "urd", "slingshot", "projected_slingshot", "dpt", "projected_dpt", "gpfates", "cellrouter", "pcreode")) %>%
  pull(method_id)

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
  if (mn %in% c("fateid", "stemnet")) {
    defaults <- tibble(id = "default", force = TRUE)
  } else {
    defaults <- tibble(id = "default")
  }
  # best <- ... %>% mutate(id = "optimised")
  # bind_rows(default, best)
  defaults
}) %>% set_names(method_ids)

##########################################################
###############       DEFINE DATASETS      ###############
##########################################################
datasets <- load_datasets() %>%
  mutate(lnrow = log10(map_dbl(cell_ids, length)), lncol = log10(map_dbl(feature_info, nrow))) %>%
  select(id, lnrow, lncol)

##########################################################
###############       CREATE DESIGN        ###############
##########################################################
design <- benchmark_generate_design(
  datasets = datasets$id,
  methods = methods,
  parameters = parameters,
  num_repeats = 1
)

# determine method execution order by predicting
# the running times of each method
predicted_times <-
  pmap_df(scaling$models, function(method_id, model, ...) {
    datasets2 <- datasets %>% rename(dataset_id = id)
    datasets2$method_id <- method_id
    datasets2$lpredtime = predict(model, datasets2)[,1]
    datasets2
  }) %>%
  mutate(predtime = 10^lpredtime)

method_ids <-
  predicted_times %>%
  group_by(method_id) %>%
  summarise(lpredtime = mean(lpredtime)) %>%
  arrange(lpredtime) %>%
  pull(method_id)

design$crossing <- design$crossing %>%
  left_join(predicted_times, by = c("method_id", "dataset_id")) %>%
  mutate(method_order = match(method_id, method_ids)) %>%
  arrange(method_order)

# save configuration
write_rds(design, derived_file("design.rds"))

##########################################################
###############        SUBMIT JOB          ###############
##########################################################
design <- read_rds(derived_file("design.rds"))

qsub_params <- function(method_id, param_id) {
  prm <- lst(timeout = 6 * 60 * 60, memory = "10G")
  if (method_id %in% c("celltree_gibbs", "scimitar", "ouijaflow", "ouija", "pseudogp", "calista", "cellrouter", "grandprix", "ouijaflow")) {
    prm$memory <- "32G"
  }
  prm
}

# submit job
benchmark_submit(
  design = design,
  qsub_grouping = "{method_id}/{param_id}",
  qsub_params = qsub_params,
  metrics = metrics_evaluated$metric_id %>% setdiff("harm_mean"),
  verbose = TRUE,
  output_models = TRUE
)
