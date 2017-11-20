library(tidyverse)

source("scripts/wouter/toy/generation.R")
source("scripts/wouter/toy/perturbation.R")

toys_blueprint <- tribble(
  ~generator_id, ~perturbator_id,
  "linear", "gs",
  "linear", "switch_two_cells",
  "linear", "switch_all_cells",
  "linear", "join_linear",
  "linear", "split_linear",
  "linear", "warp",
  "linear", "hairy",
  "linear", "hairy_small",
  "linear", "hairy_large",
  "bifurcating", "gs",
  "bifurcating", "switch_two_cells",
  "bifurcating", "switch_all_cells",
  "bifurcating", "warp",
  "bifurcating", "hairy",
  "cycle", "gs",
  "cycle", "switch_all_cells",
  "cycle", "break_cycles",
  "cycle", "warp",
  "cycle", "hairy"
) %>% rowwise() %>% mutate(
  generator=list(get(paste0("generate_", generator_id))),
  perturbator=list(get(paste0("perturb_", perturbator_id)))
)

# replicate
nreplicates <- 5
toys <- toys_blueprint %>% slice(rep(1:n(), each=nreplicates)) %>% mutate(
  replicate=seq_len(nrow(.))%%nreplicates,
  toy_category=paste0(generator_id, "-", perturbator_id),
  toy_id=paste0(toy_category, "-", replicate)
) %>%
  mutate(ncells=sample(50:500, n(), replace=TRUE))

# generate gold standards and toys, can take some time (for computing the geodesic distances I presume)
# I choose to not do this using mutate because it is much easier to debug using loops
toys$gs <- toys %>% split(seq_len(nrow(toys))) %>% parallel::mclapply(function(row) {
  row$generator[[1]](row$ncells)
}, mc.cores=8)
#toys$gs <- map2(toys$generator, toys$ncells, ~.x(.y))

toys$toy <- toys %>% split(seq_len(nrow(toys))) %>% parallel::mclapply(function(row) {
  row$perturbator[[1]](row$gs[[1]])
}, mc.cores=8)
# toys$toy <- map2(toys$perturbator, toys$gs, ~.x(.y))
toys$toy <- map2(toys$toy, toys$toy_id, ~rename_toy(.x, .y))

# plot toys
toyplots <- toys %>% group_by(toy_category) %>% filter(row_number()==1) %>%
  {split(., seq_len(nrow(.)))} %>% parallel::mclapply(function(row) dynplot::plot_strip_connections(row$gs[[1]], row$toy[[1]]), mc.cores = 8)

write_rds(toys, "toys.rds")

toys <- read_rds("toys.rds")

# get the scores when comparing the gs to toy
metrics <- c("mean_R_nx", "auc_R_nx", "Q_local", "Q_global", "correlation", "isomorphic", "net_emd", "robbie_network_score", "ged")
compare_toy <- function(gs, toy, id=toy$id) {
  scores <- dyneval:::calculate_metrics(gs, toy, metrics=metrics)$summary
  scores %>% mutate(toy_id=id)
}

# get the scores when both comparing gs to gs and comparing gs to toy
compare_gs_toy <- function(gs, toy) {
  bind_rows(
    compare_toy(gs, toy) %>% mutate(comparison="gs-toy"),
    compare_toy(gs, gs, id=toy$id) %>% mutate(comparison="gs-gs")
  )
}

scores <- toys %>% rowwise() %>% do(compare_gs_toy(.$gs, .$toy)) %>% select(-starts_with("time"))

write_rds(scores, "scores.rds")
