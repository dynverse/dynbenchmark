library(tidyverse)
library(dynwrap)
library(dyneval)
library(dynutils)

gold <- dyntoy::generate_toy_datasets(models = "bifurcating", use_tented_progressions = FALSE, num_cells = 200, num_replicates = 1) %>% extract_row_to_list(1)


## SIMPLE MERGE
pred1 <- wrap_data(
  cell_ids = gold$cell_ids
) %>% add_linear_trajectory(
  pseudotime = gold %>% add_root() %>% calculate_pseudotime()
) %>% add_cell_waypoints(100)


## INFERENCE MERGE
filter_cells <- function(traj, remove_milestones) {
  cell_ids <- traj$progressions %>% filter(!to %in% remove_milestones, !from %in% remove_milestones) %>% pull(cell_id) %>% unique()

  traj$cell_ids <- cell_ids
  traj$expression <- traj$expression[cell_ids,,drop = F]
  traj$counts <- traj$counts[cell_ids,,drop = F]

  traj <- traj %>% add_cell_waypoints(length(traj$waypoint_cells))
}

traj1 <- infer_trajectory(gold %>% filter_cells(c("M3", "M5")), "scorpius")
traj2 <- infer_trajectory(gold %>% filter_cells(c("M4", "M6")), "scorpius")

ps1 <- traj1$pseudotime
ps2 <- traj2$pseudotime
int <- intersect(names(ps1), names(ps2))
cell_ids <- gold$cell_ids

if (mean(ps1[int]) > .5) {
  ps1 <- 1 - ps1
}
if (mean(ps2[int]) > .5) {
  ps2 <- 1 - ps2
}

ps1[cell_ids] %>% set_names(cell_ids)

ps12 <- map2_dbl(ps1[cell_ids], ps2[cell_ids], function(a, b) mean(c(a, b), na.rm = T)) %>% set_names(cell_ids)

pred2 <- wrap_data(
  cell_ids = gold$cell_ids
) %>% add_linear_trajectory(
  pseudotime = ps12
) %>% add_cell_waypoints(100)


## COMPARE
cowplot::plot_grid(
  dynplot::plot_default(gold, color_cells = "milestone"),
  dynplot::plot_default(pred1, milestones = gold$milestone_ids, milestone_percentages = gold$milestone_percentages),
  dynplot::plot_default(pred2, milestones = gold$milestone_ids, milestone_percentages = gold$milestone_percentages),
  nrow = 1
)

patchwork::wrap_plots(
  list(
    dynplot::plot_linearised_comparison(gold, pred1),
    dynplot::plot_linearised_comparison(gold, pred2)
  )
)


eval <- bind_rows(
  calculate_metrics(gold, pred1) %>% mutate(type = "direct"),
  calculate_metrics(gold, pred2) %>% mutate(type = "inferred")
)
eval



waypoints <- gold$waypoint_cells
gold$geodesic_dist <- compute_tented_geodesic_distances(gold, waypoints)
pred$geodesic_dist <- compute_tented_geodesic_distances(pred, waypoints)

qplot(as.vector(gold$geodesic_dist), as.vector(pred$geodesic_dist))

qplot(as.vector(gold$geodesic_dist), as.vector(pred$geodesic_dist))

sum(as.vector(gold$geodesic_dist) == as.vector(pred$geodesic_dist) & as.vector(gold$geodesic_dist) != 0) / sum(as.vector(gold$geodesic_dist) != 0)






