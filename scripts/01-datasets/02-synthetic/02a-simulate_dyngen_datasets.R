library(qsub)
library(dyngen)
library(dynbenchmark)
library(tidyverse)

design <- crossing(
  modulenet_name = dyngen::list_modulenets(),
  platform = tibble(platform = select_platforms(10))
) %>%
  mutate(dataset_id = paste0("synthetic/dyngen/", as.character(row_number())))

qsub_config <- override_qsub_config(memory = "10G", max_wall_time = "24:00:00", num_cores = 1, name = "dyngen", wait = F)

qsub_pmap(
  design,
  simulate_dyngen,
  qsub_config = qsub_config
)
