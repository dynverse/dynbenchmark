library(dynbenchmark)
library(dyntoy)

design <- crossing(
  topology_model = names(dyntoy::network_models),
  replicate = 1:5
) %>%
  mutate(dataset_id = paste0("synthetic/dyntoy/", topology_model, "_", replicate))

pmap(design, simulate_dyntoy)
