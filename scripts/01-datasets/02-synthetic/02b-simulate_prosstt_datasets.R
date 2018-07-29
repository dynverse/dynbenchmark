library(tidyverse)
library(dynbenchmark)

# generate design of models, platforms and (randomised) splatter parameters
design <- crossing(
  topology_model = c("linear", "bifurcating", "multifurcating", "binary_tree", "tree"),
  tibble(platform = load_platforms()) %>% mutate(platform_ix = row_number())
) %>%
  mutate(
    a = runif(n(), 0.01, 0.1),
    intra_branch_tol = runif(n(), 0, 0.9),
    inter_branch_tol = runif(n(), 0, 0.9),
    alpha = exp(rnorm(n(), log(0.2), log(1.5))),
    beta = exp(rnorm(n(), log(1), log(1.5))) + 1
  ) %>%
  mutate(
    dataset_id = paste0("synthetic/prosstt/", topology_model, "_", platform_ix)
  ) %>%
  select(-platform_ix)


pmap(design, simulate_prosstt)
