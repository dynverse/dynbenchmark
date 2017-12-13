source("analysis/analyses/4-method_characterization/svg_helpers.R")

library(tidyverse)
library(dynalysis)

boxes <- tibble(
  id = c("undirected_linear", "simple_fork", "complex_fork", "undirected_cycle", "unrooted_tree", "undirected_graph"), opacity = 1
)
map(boxes$id, function(id) {
  boxes$opacity <- ifelse(boxes$id == id, 1, 0)

  change_opacity(boxes, id,figure_file("trajectory_types.svg", "trajectory_types"), folder = figure_file("mini", "trajectory_types"), export = "--export-area-drawing", trim=TRUE, svg=TRUE, png=FALSE)
})

boxes$base64 <- boxes$id %>% map_chr(function(x) {base64enc::base64encode(glue::glue("analysis/figures/trajectory_types/mini/{x}.svg"))})

saveRDS(boxes, figure_file("trajectory_type_boxes.rds", "trajectory_types"))
