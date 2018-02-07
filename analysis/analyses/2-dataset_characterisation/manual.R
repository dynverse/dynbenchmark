library(tidyverse)
library(cowplot)
library(googlesheets)
library(dynalysis)

experiment("2-dataset_characterisation/manual")

tasks <- read_rds(derived_file("tasks.rds", "2-dataset_characterisation"))

selected_tasks <- tasks[1:20, ]

load_expression <- function(expression) {
  if(class(expression) == "matrix") {
    expression
  } else {
    expression()
  }
}


##  ............................................................................
##  Plot cells                                                              ####

spaces <- pmap(as.list(selected_tasks), function(...) {
  # task <- extract_row_to_list(tasks %>% filter(category == "real"), 21)
  task <- list(...)
  print(task$id)

  task$expression <- load_expression(task$expression)

  source("../dynmodular/dimred_wrappers.R")

  plots <- list()

  space <- dimred_pca(task$expression)

  plotdata_cells <- bind_cols(space %>% as.data.frame() %>% tibble::rownames_to_column("cell_id")) %>%
    left_join(task$cell_info, by="cell_id")

  plot <- ggplot(plotdata_cells) +
    geom_point(aes(Comp1, Comp2), alpha=0.5) +
    ggraph::theme_graph()

  tibble(
    plot=list(plot),
    x_scale = max(space[, 1]) - min(space[, 1]),
    y_scale = max(space[, 2]) - min(space[, 2]),
    x_shift = min(space[, 1]),
    y_shift = min(space[, 2]),
    space=list(as.data.frame(space))
  )
}) %>% bind_rows() %>% mutate(box_id = row_number())

plots <- spaces$plot
ncol <- 5
nrow <- ceiling(length(plots)/ncol)
plots %>% cowplot::plot_grid(plotlist=., ncol=5) %>%
  cowplot::save_plot(derived_file("manual.png"), ., base_width = 4*ncol, base_height = nrow * 4)






##  ............................................................................
##  Process svg                                                             ####
library(xml2)
library(stringr)
svg <- read_xml(raw_file("wouter.svg"))
layer <- svg %>% xml_find_first("//svg:g[@inkscape:label='wouter']")
paths_svg <- layer %>% xml_find_all("svg:path") %>% xml_attr("d")

split_coords <- function(x) as.numeric(str_split(x, ",")[[1]])

# calculate dimensions of svg and layer transformation
dimensions <- c(svg %>% xml_attr("width") %>% str_sub(1, -3) %>% as.numeric(), svg %>% xml_attr("height") %>% str_sub(1, -3) %>% as.numeric())
base_coords <- layer %>% xml_attr("transform") %>% str_extract("\\(.*\\)") %>% str_sub(2, -2) %>% split_coords()

path <- paths_svg[[1]]
process_path <- function(path, base_coords = c(0, 0)) {
  # split <- path %>% gsub(" ", ",", .) %>% gsub("([A-Z])", ";\\1;", .) %>% str_split(";") %>% first()
  split <- path %>% str_split(" ") %>% first()
  i <- 1
  pieces <- list()

  while (i <= length(split)) {
    cur <- split[[i]]

    if(cur == "m") {
      relative=TRUE
    } else if (cur == "M"){
      relative=FALSE
    } else if(str_detect(cur, "[0-9\\.-]*,[0-9\\.-]")) {
      pieces <- c(pieces, list(split_coords(cur)))
      if(relative && length(pieces) > 1) {
        pieces[[length(pieces)]] <- pieces[[length(pieces)]] + pieces[[length(pieces)-1]]
      }

    } else if (cur == "c" | cur == "C") {
      start <- pieces[[length(pieces)]]
      p1 <- split_coords(split[[i+1]])
      p2 <- split_coords(split[[i+2]])
      end <- split_coords(split[[i+3]])

      if(cur == "c") {
        p1 <- p1 + start
        p2 <- p2 + start
        end <- end + start
      }

      bezier_points <- bezier::bezier(seq(0, 1, 0.1), list(p1, p2), start, end) %>% t() %>% as.data.frame() %>% as.list() %>% unname()

      pieces <- c(pieces, bezier_points)
    }

    i <- i + 1
  }
  path <- pieces %>% do.call(rbind, .) %>% as.data.frame() %>% magrittr::set_colnames(c("x", "y"))
  path$x <- path$x + base_coords[[1]]
  path$y <- path$y + base_coords[[2]]
  path %>% ggplot() + geom_path(aes(x, -y)) + coord_equal()
  path
}


paths <- tibble(
  path = map(paths_svg, process_path, base_coords)
)
paths <- paths %>% mutate(
  left = map_dbl(path, ~min(.$x)),
  top = map_dbl(path,~max(.$y)),
  bottom = map_dbl(path,~min(.$y)),
  right = map_dbl(path,~max(.$x))
)


box_width <- dimensions[[1]]/ncol
box_height <- dimensions[[2]]/nrow

paths <- paths %>% mutate(
  col1 = ceiling(left / box_width),
  col2 = ceiling(right / box_width),
  row1 = ceiling(top / box_height),
  row2 = ceiling(bottom / box_height),
  col = map2(col1, col2, c) %>% map_dbl(mean),
  row = map2(row1, row2, c) %>% map_dbl(mean),
  box_id = (row - 1) * ncol + col
)

# plot all paths
paths %>% mutate(path_id = row_number()) %>% unnest(path) %>%  ggplot(aes(x=x, y=-y, color=factor(box_id))) + geom_path(aes(group=path_id)) + coord_equal() + facet_wrap(~box_id, scales="free") + ggraph::theme_graph()

# get nodes
nodes_premerge <- paths %>%
  mutate(path_id = row_number()) %>%
  unnest(path) %>%
  mutate(node_id = row_number()) %>%
  select(node_id, path_id, box_id, x, y)

# merge nodes
node_max_dist <- 20
possible_node_merges <- (nodes_premerge$path_id %>% dist() %>% as.matrix() %>% {. != 0}) & (nodes_premerge$box_id %>% dist() %>% as.matrix() %>% {. == 0})
node_dist <- nodes_premerge %>% select(x, y) %>% dist() %>% as.matrix()
node_diagonal <- node_dist;node_diagonal[,] <- 0;diag(node_diagonal) <- 1

nodes_premerge$node_group_id <- (possible_node_merges * (node_dist < node_max_dist) + node_diagonal) %>% igraph::graph_from_adjacency_matrix() %>% igraph::components() %>% .$membership %>% as.integer()

nodes <- nodes_premerge %>%
  group_by(node_group_id) %>%
  mutate(x=mean(x), y=mean(y)) %>%
  select(-node_id) %>%
  rename(node_id = node_group_id)

# now create cluster networks and cluster centers
cluster_networks <- nodes %>%
  group_by(path_id) %>%
  summarise(
    cluster_network = list(drop_na(tibble(from=lag(node_id, 1), to=node_id))),
    box_id = as.integer(first(box_id))
) %>%
  group_by(box_id) %>%
  summarise(cluster_network = list(bind_rows(cluster_network)))


global_x_scale <- 1.2
global_y_scale <- 1.2
cluster_positions <- nodes %>%
  group_by(node_id) %>%
  summarise(
    x = first(x),
    y = first(y),
    box_id = as.integer(first(box_id))
  ) %>%
  mutate(
    x = x - box_width * ((box_id-1) %% ncol),
    y = y - box_height * floor((box_id-1) / ncol),
    y = box_height - y, # flip y because svg starts from top,
    x = x/box_width,
    y = y/box_height
  ) %>%
  left_join(spaces %>% select(x_scale, y_scale, y_shift, x_shift, box_id), by="box_id") %>%
  mutate(x = (x * x_scale + x_shift)*global_x_scale, y = (y * y_scale + y_shift) * global_y_scale) %>%
  group_by(box_id) %>%
  summarise(cluster_positions = list(tibble(x=x, y=y, node_id=node_id))) %>%
  ungroup()

spaces %>% unnest(space) %>%
  ggplot(aes(Comp1, Comp2)) +
  geom_point() +
  geom_point(aes(x, y), color="red", data=cluster_positions %>% unnest(cluster_positions)) +
  facet_wrap(~box_id, scale="free")

# now project
predictions_data <- left_join(cluster_positions, cluster_networks, "box_id") %>% left_join(spaces, "box_id")
predictions <- predictions_data %>% pmap(function(cluster_positions, cluster_network, space, ...) {
  cluster_space <- cluster_positions %>% column_to_rownames("node_id") %>% as.matrix()
  cluster_network <- cluster_network %>% mutate_all(as.character) %>% mutate(directed=F, length=1)
  sample_space <- space %>% .[, 1:2] %>% magrittr::set_colnames(c("x", "y"))

  out <- dynmethods:::project_cells_to_segments(cluster_network, cluster_space, sample_space)

  wrap_prediction_model(
    cell_ids = rownames(space),
    milestone_ids = out$milestone_ids,
    milestone_network = out$milestone_network,
    progressions = out$progressions,
    space = out$space_df,
    centers = out$centers_df,
    edge = out$edge_df
  )
})




id <- 20
task <- extract_row_to_list(tasks, id)
# task$geodesic_dist <- dynutils::compute_emlike_dist(task)
prediction <- predictions[[id]]
prediction$geodesic_dist <- dynutils::compute_emlike_dist(prediction)

dyneval::calculate_metrics(task, prediction, c("correlation", "edge_flip", "rf_mse"))$summary %>% View

dynplot::plot_default(prediction)
