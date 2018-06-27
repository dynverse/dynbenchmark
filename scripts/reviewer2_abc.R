library(tidyverse)
library(dynwrap)
library(dyneval)

cell_ids <- c(
  paste0("A", 0:9),
  paste0("B", 0:9),
  paste0("C", 0:9)
)

gold <- wrap_data(
  cell_ids = cell_ids
) %>% add_trajectory(
  milestone_ids = c("mid", "MA", "MB", "MC"),
  milestone_network = tribble(
    ~from, ~to, ~length, ~directed,
    "mid", "MA", 1, FALSE,
    "mid", "MB", 2, FALSE,
    "mid", "MC", 2, FALSE
  ),
  progressions = data_frame(cell_id = cell_ids, from = "mid", to = gsub("(.)[0-9]", "M\\1", cell_ids), percentage = runif(length(cell_ids))),
  divergence_regions = NULL
) %>% add_cell_waypoints(length(cell_ids))

pred <- wrap_data(
  cell_ids = cell_ids
) %>% add_trajectory(
  milestone_ids = c("mid", "MA", "MB"),
  milestone_network = tribble(
    ~from, ~to, ~length, ~directed,
    "mid", "MA", 1, FALSE,
    "mid", "MB", 2, FALSE
  ),
  progressions = gold$progressions %>% mutate(to = ifelse(to == "MC", "MB", to)),
  divergence_regions = NULL
) %>% add_cell_waypoints(length(cell_ids))

dynplot::plot_default(gold)
dynplot::plot_default(pred)

dynplot::plot_linearised_comparison(gold, pred)

# dynplot::plot_default(
#   traj = pred,
#   color_cells = "milestone",
#   milestones = gold$milestone_ids,
#   milestone_percentages = gold$milestone_percentages
# )

eval <- calculate_metrics(gold, pred, metrics = c("correlation", "edge_flip", "rf_mse"))
eval

waypoints <- gold$waypoint_cells
gold$geodesic_dist <- compute_tented_geodesic_distances(gold, waypoints)
pred$geodesic_dist <- compute_tented_geodesic_distances(pred, waypoints)

qplot(as.vector(gold$geodesic_dist), as.vector(pred$geodesic_dist))

qplot(as.vector(gold$geodesic_dist), as.vector(pred$geodesic_dist))

sum(as.vector(gold$geodesic_dist) == as.vector(pred$geodesic_dist) & as.vector(gold$geodesic_dist) != 0) / sum(as.vector(gold$geodesic_dist) != 0)
