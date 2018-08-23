library(dynbenchmark)
library(tidyverse)

experiment("07-benchmark")

##########################################################
###############       DEFINE METHODS       ###############
##########################################################

scaling <- read_rds(result_file("scaling.rds", "05-scaling"))

# need to look into scaling results of these methods first
method_ids <- scaling$models$method_id

methods <-
  dynwrap::get_ti_methods(method_ids, evaluate = FALSE) %>%
  mapdf(function(m) {
    l <- m$fun()
    l$fun <- m$fun
    l$type <- "function"
    l
  }) %>%
  list_as_tibble() %>%
  select(id, type, fun, everything())

default_parameters <- list(
  fateid = tibble(id = "default", force = TRUE),
  stemnet = tibble(id = "default", force = TRUE),
  tscan = tibble(id = "default", modelNames = list(c("VVV", "EEE")))
)

# combine default params and optimised params... if we had some!
parameters <- lapply(method_ids, function(mn) {
  defaults <-
    if (mn %in% names(default_parameters)) {
      default_parameters[[mn]]
    } else {
      tibble(id = "default")
    }
  # best <- ... %>% mutate(id = "optimised")
  # bind_rows(default, best)
  defaults
}) %>% set_names(method_ids)

##########################################################
###############       DEFINE DATASETS      ###############
##########################################################
datasets <-
  load_datasets() %>%
  mutate(
    lnrow = log10(map_dbl(cell_ids, length)),
    lncol = log10(map_dbl(feature_info, nrow))
  ) %>%
  select_if(is.atomic) %>%
  mutate(
    type = "character",
    fun = map(id, ~ function() load_dataset(., as_tibble = FALSE))
  )


##########################################################
###############       CREATE DESIGN        ###############
##########################################################
design <-
  benchmark_generate_design(
    datasets = datasets,
    methods = methods,
    parameters = parameters,
    num_repeats = 1
  )

# determine method execution order by predicting
# the running times of each method
predicted_times <-
  pmap_df(scaling$models, function(method_id, model_time, model_mem, ...) {
    datasets2 <- datasets %>% rename(dataset_id = id)
    datasets2$method_id <- method_id
    datasets2$lpredtime = predict(model_time, datasets2)[,1]
    datasets2$lpredmem = predict(model_mem, datasets2)[,1]
    datasets2
  }) %>%
  mutate(predtime = 10^lpredtime, predmem = 10^lpredmem)

method_ord <-
  predicted_times %>%
  group_by(method_id) %>%
  summarise(lpredtime = mean(lpredtime), lpredmem = mean(lpredmem)) %>%
  arrange(lpredtime) %>%
  pull(method_id)

design$crossing <- design$crossing %>%
  left_join(predicted_times, by = c("method_id", "dataset_id")) %>%
  mutate(method_order = match(method_id, method_ord)) %>%
  arrange(method_order)

# save configuration
write_rds(design, derived_file("design.rds"), compress = "xz")

##########################################################
###############        SUBMIT JOB          ###############
##########################################################
design_filt <- read_rds(derived_file("design.rds"))
# design_filt$crossing <- design_filt$crossing %>% filter(method_id %in% c("identity", "scorpius", "paga"))


qsub_params <- function(method_id, param_id) {
  moremem <- c(
    "celltree_gibbs", "scimitar", "ouijaflow", "ouija", "pseudogp", "calista", "cellrouter",
    "grandprix", "ouijaflow", "paga", "projected_paga", "raceid_stemid"
  )
  prm <- lst(timeout = 6 * 60 * 60, memory = "10G")
  if (method_id %in% moremem) {
    prm$memory <- "32G"
  }
  prm
}

# submit job
benchmark_submit(
  design = design_filt,
  qsub_grouping = "{method_id}/{param_id}",
  qsub_params = qsub_params,
  metrics = metrics_evaluated$metric_id %>% setdiff("harm_mean"),
  verbose = TRUE,
  output_models = TRUE
)
