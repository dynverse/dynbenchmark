library(dynbenchmark)
library(tidyverse)

experiment("05-scaling_old")

##########################################################
############### PART TWO: RETRIEVE RESULTS ###############
##########################################################

# fetch results from cluster
benchmark_fetch_results()

# bind results in one data frame (without models)
outputs <- benchmark_bind_results(load_models = FALSE)
design <- read_rds(derived_file("design.rds"))
datasets <- design$datasets


joined <-
  outputs %>%
  select(method_id, dataset_id, errored = dummy, error_status, starts_with("time_")) %>%
  left_join(datasets %>% select(dataset_id = id, lnrow, lncol, nrow, ncol), by = "dataset_id")

ggplot(joined) +
  geom_tile(aes(lnrow, lncol, fill = error_status)) +
  scale_fill_brewer(palette = "Dark2")
