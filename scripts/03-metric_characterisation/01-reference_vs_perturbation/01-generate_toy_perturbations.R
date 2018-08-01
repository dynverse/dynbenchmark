## Generation of toy gold standards and perturbations

library(dynbenchmark)
library(tidyverse)

experiment("03-metric_characterisation/01-reference_vs_perturbation")



toys_blueprint <- tribble(
  ~topology_model, ~perturbator_id,
  "simple_linear", "gs",
  "simple_linear", "switch_two_cells",
  "simple_linear", "switch_all_cells",
  "simple_linear", "join_linear",
  "simple_linear", "split_linear",
  "simple_linear", "warp",
  "simple_linear", "hairy",
  "simple_linear", "hairy_small",
  "simple_linear", "hairy_large",
  "simple_linear", "remove_cells",
  "simple_linear", "add_distant_edge",
  "simple_linear", "structure_and_position",
  "bifurcating", "gs",
  "bifurcating", "change_lengths",
  "bifurcating", "change_terminal_lengths",
  "bifurcating", "switch_two_cells",
  "bifurcating", "switch_all_cells",
  "bifurcating", "warp",
  "bifurcating", "hairy",
  "bifurcating", "remove_cells",
  "bifurcating", "add_distant_edge",
  "bifurcating", "structure_and_position",
  "cycle", "gs",
  "cycle", "switch_all_cells",
  "cycle", "break_cycles",
  "cycle", "warp",
  "cycle", "hairy",
  "cycle", "remove_cells",
  "cycle", "add_distant_edge"
) %>%
  bind_rows(tibble(trajectory_model="simple_linear", perturbator_id=paste0("switch_", percs * 100))) %>%
  bind_rows(crossing(trajectory_model=trajectory_models, perturbator_id=paste0("change_network_", trajectory_models)) %>% mutate(ncells = 5)) %>%
  rowwise() %>%
  mutate(
    perturbator=list(get(paste0("perturb_", perturbator_id)))
  )

# replicate
nreplicates <- 1
toys <- toys_blueprint %>% slice(rep(1:n(), each=nreplicates)) %>% mutate(
  replicate=seq_len(nrow(.))%%nreplicates,
  toy_category=paste0(trajectory_model, "-", perturbator_id),
  toy_id=paste0(toy_category, "-", replicate)
) %>%
  mutate(ncells=sample(50:500, n(), replace=TRUE))

# generate gold standards and toys, can take some time (for computing the geodesic distances I presume)
# I choose to not do this using mutate because it is much easier to debug using loops
toys$gs <- toys %>% split(seq_len(nrow(toys))) %>% parallel::mclapply(function(row) {
  dyntoy:::generate_dataset("why", row$trajectory_model, row$ncells)
}, mc.cores=8)
#toys$gs <- map2(toys$generator, toys$ncells, ~.x(.y))

toys$toy <- toys %>% split(seq_len(nrow(toys))) %>% parallel::mclapply(function(row) {
  row$perturbator[[1]](row$gs[[1]])
}, mc.cores=8)
# toys$toy <- map2(toys$perturbator, toys$gs, ~.x(.y))
toys$toy <- map2(toys$toy, toys$toy_id, ~rename_toy(.x, .y))

toys$gs_grouped <- parallel::mclapply(toys$gs, group_dataset, mc.cores=8)

# plot toys
toyplots <- toys %>% group_by(toy_category) %>% filter(row_number()==1) %>%
{split(., seq_len(nrow(.)))} %>% parallel::mclapply(function(row) dynplot::plot_strip_connections(row$gs[[1]], row$toy[[1]]), mc.cores = 8)

write_rds(toys, derived_file("toys.rds"))
