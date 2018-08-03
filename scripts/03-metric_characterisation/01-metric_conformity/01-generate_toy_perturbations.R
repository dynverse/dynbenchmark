## Generation of toy gold standards and perturbations

library(dynbenchmark)
library(tidyverse)
library(dyntoy)

experiment("03-metric_characterisation/01-reference_vs_perturbation")

## create datasets
datasets_design <- crossing(
  topology_model = names(topology_models),
  num_cells = c(2, seq(100, 1000, by = 1000)),

) %>%
  mutate(
    dataset_id = str_glue("{topology_model}_{num_cells}")
  )

###
metrics <- formals(calculate_metrics)$metrics %>% eval %>% as.list() %>% {set_names(., as.character(.))} %>% discard(~. == "featureimp_cor")


assess_defined <- function(datasets_design, metrics) {
  perturbation_design <- crossing(
    dataset_id = datasets_design$dataset_id,
    perturbator_id = "gs"
  )
  scores_summary <- score_perturbation_design(perturbation_design)

  lst(
    assessment = scores_summary %>%
      group_by(metric_id) %>%
      summarise(check = all(!is.na(score))) %>%
      mutate(rule_id = "defined"),
    plot = scores_summary %>%
      ggplot(aes())
  )
}


assess_equal_gold_standard_score <- function(datasets_design, metrics) {
  scores_summary <- crossing(
    datasets_design,
    perturbator_id = "gs"
  ) %>% score_perturbation_design()

  approx_unique <- function(x, tolerance = .Machine$double.eps ^ 0.5) {
    abs(first(sort(x)) - last(sort(x))) < tolerance
  }

  lst(
    assessment = scores_summary %>%
      group_by(metric_id) %>%
      summarise(check = approx_unique(score)) %>%
      mutate(rule_id = "equal_gold_standard_score")
    ,
    plot = scores_summary %>%
      ggplot(aes(metric_id, score)) +
      ggbeeswarm::geom_quasirandom()
  )
}

assess_switch_cells <- function(datasets_design, metrics) {
  scores_summary <- crossing(
    datasets_design,
    perturbator_id =
  ) %>% score_perturbation_design()
}

assessment <- assess_defined(datasets_design)
assessment <- assess_equal_gold_standard_score(datasets_design)



















design <- crossing(
  topology_model = names(topology_models),
  num_cells = c(2, seq(50, 1000, length.out = 10))
) %>% mutate(

)

score_design <- function(topology_model, num_cells, perturbator_id, ...) {
  print("--")



}

scores <- pmap(design, score_design)





design_manual <- tribble(
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
)

design_switch_n <- tibble(
  topology_model="simple_linear",
  switch_cells_perc = seq(0, 1, 0.1),
  perturbator_id = "switch_perc_cells"
)

design_topology_changes <- crossing(
  topology_model=names(topology_models),
  perturbator_id=paste0("change_network_", names(topology_models))
) %>% mutate(num_cells = 5)

num_cells <-
design <- crossing(
  bind_rows(
    design_manual,
    design_switch_n,
    design_topology_changes
  ),
  tibble(num_cells = c)
)

# replicate

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
