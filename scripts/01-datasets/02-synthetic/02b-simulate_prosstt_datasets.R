#' [PROSSTT](https://github.com/soedinglab/prosstt), expression is sampled from a linear model which depends on pseudotime

library(tidyverse)
library(dynbenchmark)
library(qsub)

experiment("01-datasets/02-synthetic")

# generate design of models, platforms and (randomised) splatter parameters
design <- crossing(
  topology_model = c("linear", "bifurcating", "multifurcating", "binary_tree", "tree"),
  tibble(platform = select_platforms(10)) %>% mutate(platform_ix = row_number())
) %>%
  mutate(
    a = runif(n(), 0.01, 0.1),
    intra_branch_tol = runif(n(), 0, 0.9),
    inter_branch_tol = runif(n(), 0, 0.9),
    alpha = exp(rnorm(n(), log(0.3), log(1.5))),
    beta = exp(rnorm(n(), log(2), log(1.5))) + 1
  ) %>%
  mutate(
    dataset_id = paste0("synthetic/prosstt/", topology_model, "_", platform_ix),
    seed = sample(1:100000, n())
  ) %>%
  select(-platform_ix)
write_rds(design, result_file("design_prosstt.rds"))

# simulate datasets
qsub_config <- override_qsub_config(memory = "10G", max_wall_time = "24:00:00", num_cores = 1, name = "prosstt", wait = F, execute_before = "module load python/x86_64/3.6.5", stop_on_error = FALSE)

handle <- qsub_pmap(
  design,
  simulate_prosstt,
  qsub_config = qsub_config
)

write_rds(handle, "handle_prosstt.rds")

##
handle <- read_rds("handle_prosstt.rds")
qsub::qsub_retrieve(handle)
