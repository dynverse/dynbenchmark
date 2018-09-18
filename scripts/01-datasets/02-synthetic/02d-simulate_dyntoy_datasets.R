#' [dyntoy](https://github.com/dynverse/dyntoy), simulations of toy data using random expression gradients in a reduced space

library(dynbenchmark)
library(dyntoy)
library(tidyverse)
library(qsub)

experiment("01-datasets/02-synthetic")

# remove all datasets
# rm_remote(dataset_file(id = "synthetic/dyntoy", remote = TRUE), remote = TRUE, recursive = TRUE)

# generate design
set.seed(1)
design <- crossing(
  topology_model = names(dyntoy::topology_models),
  tibble(platform = select_platforms(10)) %>% mutate(platform_ix = row_number())
) %>%
  mutate(
    dataset_id = paste0("synthetic/dyntoy/", topology_model, "_", platform_ix),
    seed = sample(1:100000, n())
  ) %>%
  select(-platform_ix)
write_rds(design, result_file("design_dyntoy.rds"))

# simulate datasets
qsub_config <- override_qsub_config(memory = "10G", max_wall_time = "24:00:00", num_cores = 1, name = "dyntoy", wait = F, stop_on_error = FALSE)

handle <- qsub_pmap(
  design,
  simulate_dyntoy,
  qsub_config = qsub_config
)

write_rds(handle, "handle_dyntoy.rds")

handle <- read_rds("handle_dyntoy.rds")
qsub::qsub_retrieve(handle)
