library(tidyverse)
library(dynbenchmark)
library(qsub)

design <- crossing(
  topology_model = c("linear", "bifurcating", "multifurcating", "binary_tree", "tree"),
  tibble(platform = load_platforms()) %>% mutate(platform_ix = row_number())
) %>%
  mutate(
    path.skew = runif(n(), 0, 1),
    path.nonlinearProb = runif(n(), 0, 1),
    path.sigmaFac = runif(n(), 0, 1),
    bcv.common.factor = runif(n(), 10, 200),
    dataset_id = paste0("synthetic/splatter/", topology_model, "_", platform_ix)
  ) %>%
  select(-platform_ix)

qsub_config <- override_qsub_config(memory = "10G", max_wall_time = "24:00:00", num_cores = 1, name = "dyngen", wait = F)

qsub_pmap(
  design,
  simulate_splatter,
  qsub_config = qsub_config
)
