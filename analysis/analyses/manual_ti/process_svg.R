library(dynalysis)
library(png)
library(tidyverse)
library(GNG)
library(tidygraph)
library(ggraph)

experiment("manual_ti")


##  ............................................................................
##  Load run                                                                ####

run_id <- "mds_rcannood_1"
run <- read_rds(derived_file(paste0(run_id, ".rds")))
run$spaces <- run$spaces[[1]]


##  ............................................................................
##  Load bitmap                                                             ####

processed_file <- derived_file(run_id, '_run', '.png')
processx::run(commandline=dynutils::pritt("inkscape -z {derived_file(run_id, '.svg')} -e={processed_file} -d=100"))

png <- readPNG(processed_file)
red <- png[, , 1] - png[, , 2]


##  ............................................................................
##  Get coordinates from bitmap                                             ####
coordinates <- red %>%
  reshape2::melt(varnames=c("y", "x"), value.var=value) %>%
  as_tibble() %>%
  filter(value > 0.5)

box_height <- dim(red)[[1]]/run$nrow
box_width <- dim(red)[[2]]/run$ncol

coordinates <- coordinates %>%
  mutate(
    col = floor((x-1)/box_width) + 1,
    row = floor((y-1)/box_height),
    box_id =  (row * run$ncol) + col
  )
coordinates %>% filter(y<200) %>%  ggplot() + geom_point(aes(x, -y, color=factor(box_id)))



##  ............................................................................
##  Generate initial GNG from coordinates                                   ####
process_coordinates <- function(coordinates) {
  data <- coordinates %>% select(x, y) %>% as.matrix()
  result <- GNG::gng(data, max_nodes=50, assign_cluster=F)

  graph <- result$edges %>% as_tbl_graph() %>%
    activate(nodes) %>%
    left_join(result$nodes %>% mutate(index=as.character(index)) %>% rename(id=name), by=c("name"="index")) %>%
    left_join(result$node_space %>% as.data.frame() %>% tibble::rownames_to_column("id"), "id")
  graph
}

coordinates_split <- coordinates %>% split(coordinates$box_id)
graphs <- tibble(graph = map(coordinates_split, process_coordinates), box_id = as.numeric(names(coordinates_split)))
graphs <- graphs %>% left_join(run$spaces, by="box_id")

map(graphs$graph[16:16], function(graph) {
  ggraph(graph) +
    geom_node_point() +
    geom_edge_link()
}) %>% cowplot::plot_grid(plotlist=.)


##  ............................................................................
##  Merge GNG nodes if too close                                            ####

merge_graph <- function(graph, max_dist) {
  distances <- graph %>% activate(nodes) %>% as_tibble() %>% select(x, y) %>% as.matrix() %>% dist() %>% as.matrix()
  rownames(distances) <- colnames(distances) <- graph %>% pull(name)
  close <- distances < maxdist

  n_close <- close %>% apply(1, sum)
  close <- close[order(n_close, decreasing=T), order(n_close, decreasing=T)]
  labels <- setNames(rownames(distances), rownames(distances))

  for (i in rownames(close)) {
    which_close <- names(which(close[i, ]))
    close[which_close, ] <- FALSE
    close[, which_close] <- FALSE
    labels[which_close] <- i
  }

  nodes <- graph %>%
    activate(nodes) %>%
    as_tibble() %>%
    mutate(new = labels) %>%
    group_by(new) %>%
    summarise(x = mean(x), y=mean(y)) %>%
    rename(name=new)
  edges <- graph %>%
    activate(edges) %>%
    as_tibble() %>%
    mutate(from = labels[from], to = labels[to]) %>%
    mutate(from = match(from, nodes$name), to = match(to, nodes$name)) %>%
    filter(from!=to) %>%
    group_by(from, to) %>%  # filter duplicates
    filter(row_number() == 1) %>%
    ungroup()

  new_graph <- tbl_graph(nodes, edges)

  # new_graph%>%
  #   ggraph() +
  #   geom_node_point() +
  #   geom_edge_link()

  new_graph
}

maxdist <- box_width / 10

graphs$graph_merged <- map(graphs$graph, merge_graph, maxdist)

map(graphs$graph_merged[1:20], function(graph) {
  ggraph(graph) +
    geom_node_point() +
    geom_edge_link()
}) %>% cowplot::plot_grid(plotlist=.)

##  ............................................................................
##  Project onto edges                                                      ####


### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Rescale to match space                                                  ####
global_x_scale <- 1.15
global_y_scale <- 1.15

scale_graph <- function(graph, box_id) {
  space <- run$spaces %>% filter(box_id == !!box_id)

  graph %>%
    activate(nodes) %>%
    mutate(
      x = x - box_width * ((box_id-1) %% run$ncol),
      y = y - box_height * floor((box_id-1) / run$ncol),
      y = box_height - y, # flip y because svg starts from top,
      x = x/box_width,
      y = y/box_height
    ) %>%
    mutate( # scale from center
      x = (x - 0.5) * global_x_scale + 0.5,
      y = (y - 0.5) * global_y_scale + 0.5
    ) %>%
    mutate(
      x = x * space$x_scale + space$x_shift,
      y = y * space$y_scale + space$y_shift
    )
}

graphs$graph_scaled <- map2(graphs$graph_merged, graphs$box_id, scale_graph)

graphs[100:110, ] %>% pmap(function(graph_scaled, space, ...) {
  ggraph(graph_scaled) +
    geom_point(aes(Comp1, Comp2), data=space) +
    geom_edge_link(edge_colour="darkred", edge_width=4)+
    geom_node_point(color="red")
}) %>% cowplot::plot_grid(plotlist=.)


### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Project                                                                 ####
predictions_list <- graphs %>% pmap(function(box_id, graph_scaled, space, ...) {
  print(box_id)
  cluster_space <- graph_scaled %>%
    activate(nodes) %>%
    as_tibble() %>%
    select(x, y) %>%
    as.matrix()
  rownames(cluster_space) <- seq_len(nrow(cluster_space))

  cluster_network <- graph_scaled %>%
    activate(edges) %>%
    as_tibble() %>%
    mutate_all(as.character) %>%
    mutate(directed=F, length=1)

  sample_space <- space %>% .[, c("Comp1", "Comp2")] %>% magrittr::set_colnames(c("x", "y")) %>% magrittr::set_rownames(space$cell_id)

  out <- dynmethods:::project_cells_to_segments(cluster_network, cluster_space, sample_space)

  wrap_prediction_model(
    cell_ids = space$cell_id,
    milestone_ids = out$milestone_ids,
    milestone_network = out$milestone_network,
    progressions = out$progressions,
    space = out$space_df,
    centers = out$centers_df,
    edge = out$edge_df
  )
})
predictions <- tibble(prediction = predictions_list, task_id = graphs$id)



### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Check scores of controls                                                ####
tasks <- read_rds(derived_file("tasks.rds", experiment_id="2-dataset_characterisation"))
tasks <- tasks %>% slice(match(run$spaces$id, id))

controls <- map(tasks %>% filter(task_group == "control" & id != "control_BA") %>% pull(id), function(task_id) {
  print(task_id)
  task <- extract_row_to_list(tasks, which(tasks$id == task_id))
  task$geodesic_dist <- dynutils::compute_tented_geodesic_distances(task)

  prediction <- extract_row_to_list(predictions, which(predictions$task_id == task_id))$prediction
  prediction$geodesic_dist <- dynutils::compute_tented_geodesic_distances(prediction)

  plot = dynplot::plot_default(prediction)
  plot

  tibble(
    scores = dyneval::calculate_metrics(task, prediction, c("correlation", "edge_flip", "rf_mse"))$summary %>%
      select(correlation, edge_flip) %>% list(),
    plot = list(plot),
    task_id = task_id
  ) %>% unnest(scores)
}) %>% bind_rows()

controls


##  ............................................................................
##  Save                                                                    ####
predictions %>% write_rds(derived_file(pritt("predictions_{run$run_id}.rds")))

# upload to PRISM
PRISM:::rsync_remote(
  remote_dest = "prism",
  path_dest = derived_file() %>% gsub(dynalysis::get_dynalysis_folder(), PRISM:::run_remote("echo $DYNALYSIS_PATH", "prism")$cmd_out, .),
  remote_src = "",
  path_src = derived_file()
)
