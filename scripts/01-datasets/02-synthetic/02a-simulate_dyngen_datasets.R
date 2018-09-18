#' [dyngen](https://github.com/dynverse/dyngen), simulations of regulatory networks which will produce a particular trajectory

library(qsub)
library(dyngen)
library(dynbenchmark)
library(tidyverse)

experiment("01-datasets/02-synthetic")

# remove all datasets
# rm_remote(dataset_file(id = "synthetic/dyngen", remote = TRUE), remote = TRUE, recursive = TRUE)

# create design
set.seed(1)
design <- crossing(
  modulenet_name = dyngen::list_modulenets(),
  tibble(platform = select_platforms(10)) %>% mutate(platform_ix = row_number())
) %>%
  mutate(
    dataset_id = paste0("synthetic/dyngen/", row_number()),
    seed = sample(1:100000, n())
  ) %>%
  select(-platform_ix)
write_rds(design, result_file("design_dyngen.rds"))

# simulate datasets
qsub_config <- override_qsub_config(memory = "10G", max_wall_time = "24:00:00", num_cores = 1, name = "dyngen", wait = F, stop_on_error = FALSE)

handle <- qsub_pmap(
  design,
  simulate_dyngen,
  use_cache = TRUE,
  qsub_config = qsub_config
)

write_rds(handle, "handle_dyngen.rds")

##
handle <- read_rds("handle_dyngen.rds")
qsub::qsub_retrieve(handle)
