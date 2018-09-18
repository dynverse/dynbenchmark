#' Gathers some metadata about all the synthetic datasets

library(dynbenchmark)
library(googlesheets)

experiment("01-datasets/02-synthetic")

datasets_synthetic <- load_datasets(list_datasets() %>% filter(startsWith(source, "synthetic/")) %>% pull(id))

datasets_synthetic_metadata <- datasets_synthetic$simulation_design %>%
  list_as_tibble()

write_rds(datasets_synthetic_metadata, result_file("metadata.rds"))

