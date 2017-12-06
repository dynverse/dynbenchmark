source("analysis/analyses/4-method_characterization/svg_helpers.R")

boxes <- tibble(
  id = c("linear", "single_bifurcation", "binary_tree", "single_multifurcation", "non_binary_tree", "single_cycle", "simple_graph", "single_bifurcation_single_convergence"), opacity = 1
)
map(boxes$id, function(id) {
  boxes$opacity <- ifelse(boxes$id == id, 1, 0)

  change_opacity(boxes, id, "analysis/figures/trajectory_types.svg", folder = "analysis/figures/trajectory_types/mini/", export = "--export-area-drawing", trim=TRUE, svg=TRUE, png=FALSE)
})
