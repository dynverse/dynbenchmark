list2env(dataset, .GlobalEnv)


Coi <- apply(expression, 1, mean) %>% {which(.>0)} %>% names
Goi <- apply(expression[Coi, ], 2, sd) %>% sort(decreasing = TRUE) %>% names() %>% head(2000)
pheatmap::pheatmap(t(SCORPIUS::scale_quantile(expression[Coi,Goi])), annotation_col = cell_grouping %>% tibble::column_to_rownames("cell_id") %>% select(group_id))

cell_info$num_genes_expressed <- counts %>% apply(1, function(x) sum(x>0))
cell_info$num_reads <- counts %>% apply(1, function(x) sum(x>0))
cell_info %>% ggplot() + geom_boxplot(aes(milestone_id, num_reads))

source("../dynmodular/dimred_wrappers.R");space = dimred_lmds(expression[Coi, Goi], ndim = 2)
space = SCORPIUS::reduce_dimensionality(SCORPIUS::correlation_distance(expression[Coi, Goi]), ndim=2)
source("../dynmodular/dimred_wrappers.R");space = dimred_ica(expression[Coi, Goi], ndim = 2)
traj = SCORPIUS::infer_trajectory(space)
SCORPIUS::draw_trajectory_plot(space, cell_info %>% slice(match(Coi, cell_id)) %>% .$milestone_id, traj$path)
SCORPIUS::draw_trajectory_plot(space, expression[Coi,Goi[1]], traj$path) + viridis::scale_color_viridis()
SCORPIUS::draw_trajectory_plot(space, expression[Coi,"PDGFRA"], traj$path) + viridis::scale_color_viridis()
SCORPIUS::draw_trajectory_plot(space, rowMeans(expression[Coi, ]), traj$path) + viridis::scale_color_viridis()



source("../dynmodular/dimred_wrappers.R");space = dimred_ica(expression[Coi, Goi], ndim = 3)
library(rgl)
assignment_int <- as.numeric(factor(cell_grouping$group_id))
plot3d(space[, 1], space[, 2], space[, 3], col=rainbow(max(assignment_int))[assignment_int])




expression_grouped <- expression[Coi, Goi] %>% group_expression(cell_grouping)
score_milestone_transitions(expression_grouped, milestone_ids, milestone_network)
score_milestone_grouping(expression[Coi, Goi], cell_grouping)
score_milestone_connectivity(expression[Coi, Goi], cell_grouping, milestone_ids, milestone_network)



igraph::graph_from_data_frame(milestone_network) %>% plot
