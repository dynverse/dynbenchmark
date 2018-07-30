library(dynbenchmark)
library(dyntoy)

design <- crossing(
  topology_model = names(dyntoy::network_models),
  # tibble(platform = select_platforms(10)) %>% mutate(platform_ix = row_number()),
  tibble(platform = list(load_simple_platform())) %>% mutate(platform_ix = row_number())
) %>%
  mutate(
    dataset_id = paste0("synthetic/dyntoy/", topology_model, "_", platform_ix),
    seed = sample(1:100000, n())
  ) %>%
  select(-platform_ix)

dataset <- pmap(design[4, ], simulate_dyntoy)
dataset[[1]]$simulation_design
