library(dynalysis)
library(tidyverse)
library(tidygraph)
library(ggraph)

experiment("datasets/control")

#### update dyncontrol if you want to use this



# models <- formals(dyntoy:::generate_toy_milestone_network)$model
#
# tasks <- map(models, function(model) {
#   print(model)
#
#   id <- paste0("control/", model)
#
#   set.seed(1)
#   milestone_network <- dyntoy:::generate_toy_milestone_network(model)
#   milestone_ids <- unique(c(milestone_network$from, milestone_network$to))
#
#   # now subdivide the edges further into segments
#   milestone_segment_network <- milestone_network %>%
#     mutate(extra_milestones = map(length, ~paste0(dynutils::random_time_string(), 1:round(sample(40:100, 1)*.)))) %>%
#     mutate(extra_milestones = map2(extra_milestones, from, ~c(.y, .))) %>%
#     mutate(extra_milestones = map2(extra_milestones, to, ~c(., .y))) %>%
#     mutate(froms = map(extra_milestones, ~.[-length(.)]), tos=map(extra_milestones, ~.[-1]), percentage = map(extra_milestones, ~seq(0, 1, length.out = length(.)-1))) %>%
#     unnest(froms, tos, percentage) %>%
#     drop_na() %>%
#     rename(from_original=from, to_original=to) %>%
#     rename(from=froms, to=tos) %>%
#     select(from, to, length, directed, from_original, to_original, percentage) %>%
#     mutate(length = length/npieces)
#
#   # create layout
#   milestone_graph <- milestone_segment_network %>% as_tbl_graph()
#
#   set.seed(1)
#   if(model %in% c("linear", "simple_linear", "linear_long")) {
#     milestone_layout <- create_layout(milestone_graph, "kk", maxiter=100000)
#   } else if (model %in% c("trifurcating")) {
#     milestone_layout <- create_layout(milestone_graph, "kk", maxiter=100000)
#   } else {
#     milestone_layout <- create_layout(milestone_graph, "mds")
#   }
#
#   # plot layout
#   ggraph(milestone_layout) + geom_edge_link(aes(color=percentage))
#
#   # now disperse the points
#   set.seed(1)
#   scale <- function(x) (x- min(x)) / (max(x) - min(x))
#   disperse <- function(x, sd = 0.02) x + rnorm(sd=sd, length(x))
#   space <- milestone_layout %>%
#     mutate_at(vars(x, y), scale) %>%
#     mutate_at(vars(x, y), disperse) %>%
#     rename(Comp1=x, Comp2=y, cell_id = name) %>%
#     mutate(cell_id = as.character(cell_id))
#
#   space %>% ggplot(aes(Comp1, Comp2)) + geom_point()
#
#   # create progressions
#   progressions <- milestone_segment_network %>%
#     select(-to) %>%
#     rename(cell_id = from, from=from_original, to=to_original) %>%
#     select(cell_id, from, to, percentage)
#
#   # create counts
#   counts <- space[, c("Comp1", "Comp2")] %>% as.matrix()
#   rownames(counts) <- space$cell_id
#
#   task <- wrap_ti_task_data(
#     id,
#     counts=counts,
#     expression=counts,
#     cell_ids = space$cell_id,
#     milestone_ids = milestone_ids,
#     milestone_network = milestone_network,
#     progressions = progressions,
#     space=space,
#     task_group="control"
#   )
#
#   task$prior_information <- dynwrap::generate_prior_information(milestone_ids, milestone_network, progressions, task$milestone_percentages, counts, task$feature_info, task$cell_info)
#
#   task
# }) %>% dynutils::list_as_tibble()
#
# tasks$space %>% map(~ggplot(as.data.frame(.), aes(Comp1, Comp2)) + geom_point()) %>% cowplot::plot_grid(plotlist=.)
#
#
# write_rds(tasks, dataset_file("tasks.rds", dataset_id="control"))
