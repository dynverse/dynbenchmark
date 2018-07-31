library(qsub)
library(dyngen)
library(dynbenchmark)
library(tidyverse)

# remove all datasets
rm_remote(dataset_file(dataset_id = "synthetic/dyngen", remote = TRUE), remote = TRUE, recursive = TRUE)

# create design
set.seed(1)
design <- crossing(
  modulenet_name = dyngen::list_modulenets(),
  platform = tibble(platform = select_platforms(10))
) %>%
  mutate(
    dataset_id = paste0("synthetic/dyngen/", as.character(row_number())),
    seed = sample(1:100000, n())
  )

# simulate datasets
qsub_config <- override_qsub_config(memory = "10G", max_wall_time = "24:00:00", num_cores = 1, name = "dyngen", wait = F)

handle <- qsub_pmap(
  design,
  simulate_dyngen,
  use_cache = FALSE,
  qsub_config = qsub_config
)

write_rds(handle, "handle_dyngen.rds")

##
handle <- read_rds("handle_dyngen.rds")
qsub::qsub_retrieve(handle)
