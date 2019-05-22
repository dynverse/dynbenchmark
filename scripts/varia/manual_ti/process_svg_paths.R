library(tidyverse)
library(cowplot)
library(googlesheets)
library(dynbenchmark)

library(xml2)
library(stringr)

experiment("manual_ti")

run_id <- "mds_robrechtc_1"
run <- read_rds(derived_file(paste0(run_id, ".rds")))
run$spaces <- run$spaces[[1]]

run$spaces$id <- datasets$id[pmatch(run$spaces$id %>% gsub(".*/(.*)", "\\1", .), datasets$id %>% gsub(".*/(.*)", "\\1", .))]



##  ............................................................................
##  Process svg                                                             ####
# make sure to
# - remove beziers using 1. select all paths 2. extensions -> modify paths -> flatten bezier 3. Move all up and down to refresh data
# - The images dimensions are in points

svg <- read_xml(derived_file(glue::glue("{run$run_id}.svg")))
layer <- svg %>% xml_find_first("//svg:g[@inkscape:label = 'segments']")
paths_svg <- layer %>% xml_find_all("svg:path") %>% xml_attr("d")

split_coords <- function(x) as.numeric(str_split(x, ",")[[1]])

# calculate dimensions of svg and layer transformation
dimensions <- c(
  svg %>% xml_find_first("svg:g[@inkscape:label = 'back']") %>% xml_child() %>% xml_attr("width") %>% as.numeric(),
  svg %>% xml_find_first("svg:g[@inkscape:label = 'back']") %>% xml_child() %>% xml_attr("height") %>% as.numeric()
)
if (any(is.na(dimensions))) {stop("Dimensions should be in pt!!")}
base_coords <- layer %>% xml_attr("transform") %>% str_extract("\\(.*\\)") %>% str_sub(2, -2) %>% split_coords()
if(is.na(base_coords)) base_coords <- c(0, 0)



### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Process paths                                                           ####
path <- paths_svg[[1]]

extract_first_consecutive <- function(x) {
  if(all(diff(x) == 1)) {
    x
  } else {
    x[1:first(which(diff(x) != 1))]
  }
}

process_path <- function(path, base_coords = c(0, 0)) {
  # split <- path %>% gsub(" ", ",", .) %>% gsub("([A-Z])", ";\\1;", .) %>% str_split(";") %>% first()
  split <- path %>% str_split(" ") %>% first()
  i <- 1
  pieces <- list()

  while (i <= length(split)) {
    cur <- split[[i]]

    if(cur %in% c("m", "l")) {
      relative = TRUE
    } else if (cur %in% c("M", "L")){
      relative = FALSE
    } else if(str_detect(cur, "[0-9\\.-]*,[0-9\\.-]")) {
      pieces <- c(pieces, list(split_coords(cur)))
      if(relative && length(pieces) > 1) {
        pieces[[length(pieces)]] <- pieces[[length(pieces)]] + pieces[[length(pieces)-1]]
      }
    } else if (cur %in% c("h")) {
      horizontals <- which(str_detect(split, "^[\\-\\.0-9]*$")) %>% {.[.>i]} %>% extract_first_consecutive
      split[horizontals] <- paste0(split[horizontals], ",0")
    } else if (cur %in% c("v")) {
      verticals <- which(str_detect(split, "^[\\-\\.0-9]*$")) %>% {.[.>i]} %>% extract_first_consecutive
      split[verticals] <- paste0("0,", split[verticals])
    } else if (cur %in% c("H")) {
      print(path)
      relative <- F
      split[[i + 1]] <- paste0(split[[i + 1]], ",", pieces[[length(pieces)]][[2]])
    } else if (cur %in% c("V")) {
      relative <- F
      split[[i + 1]] <- paste0("0,", split[[i + 1]])
    } else if (cur %in% c("l")) {
      relative <- T
    }else {
      print(cur)
      print(path)
      stop("Invalid line")
    }

      # } else if (cur == "c" | cur == "C") {
      #   start <- pieces[[length(pieces)]]
      #   p1 <- split_coords(split[[i+1]])
      #   p2 <- split_coords(split[[i+2]])
      #   end <- split_coords(split[[i+3]])
      #
      #   if(cur == "c") {
      #     p1 <- p1 + start
      #     p2 <- p2 + start
      #     end <- end + start
      #   }
      #
      #   bezier_points <- bezier::bezier(seq(0, 1, 0.1), list(p1, p2), start, end) %>% t() %>% as.data.frame() %>% as.list() %>% unname()
      #
      #   pieces <- c(pieces, bezier_points)
      # }

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


### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Put paths into boxes                                                    ####
paths <- paths %>% mutate(
  left = map_dbl(path, ~min(.$x)),
  top = map_dbl(path,~max(.$y)),
  bottom = map_dbl(path,~min(.$y)),
  right = map_dbl(path,~max(.$x))
)

box_width <- dimensions[[1]]/run$ncol
box_height <- dimensions[[2]]/run$nrow

paths <- paths %>% mutate(
  col1 = ceiling(left / box_width),
  col2 = ceiling(right / box_width),
  row1 = ceiling(top / box_height),
  row2 = ceiling(bottom / box_height),
  col = map2(col1, col2, c) %>% map_dbl(mean),
  row = map2(row1, row2, c) %>% map_dbl(mean),
  box_id = (row - 1) * run$ncol + col
)

if(!all(run$spaces$box_id %in% paths$box_id)) {
  print(which(!run$spaces$box_id %in% paths$box_id))
  stop("Not all boxes have lines")
}

if(any(as.integer(paths$box_id) != paths$box_id)) {
  stop("Paths overlap between boxes!")
}

# intermediate plot check, make sure all paths look ok
paths[paths$box_id <= 10 & paths$box_id >= 1,] %>%
  mutate(path_id = row_number()) %>%
  unnest(path) %>%
  ggplot(aes(x = x, y = -y, color = factor(box_id))) + geom_path(aes(group = path_id)) + coord_equal() + facet_wrap(~box_id, scales = "free") + ggraph::theme_graph() + theme(legend.position = "none")


### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Merge close nodes                                                       ####

# get nodes
nodes_premerge <- paths %>%
  mutate(path_id = row_number()) %>%
  unnest(path) %>%
  mutate(node_id = row_number()) %>%
  select(node_id, path_id, box_id, x, y)

# merge nodes
node_max_dist <- 5

# allow three types of merges: of end points (within the same box id and path id), of different paths, or on the diagonal (node merges with itself obviously)
same_box_id <- (nodes_premerge$box_id %>% dist() %>% as.matrix() %>% {. == 0})

end_nodes <- (c(0, nodes_premerge$path_id) %>% diff()) %>% {. + lead(., 1)}
possible_node_merges_end <- (end_nodes %*% t(rep(1, length(end_nodes)))) & same_box_id

possible_node_merges_path <- (nodes_premerge$path_id %>% dist() %>% as.matrix() %>% {. != 0}) & same_box_id

possible_node_merges_diagonal <- possible_node_merges_path;possible_node_merges_diagonal[,] <- 0;diag(possible_node_merges_diagonal) <- 1

node_dist <- nodes_premerge %>% select(x, y) %>% dist() %>% as.matrix()
possible_node_merges <- possible_node_merges_end | possible_node_merges_path | possible_node_merges_diagonal

nodes_premerge$node_group_id <- (possible_node_merges * (node_dist < node_max_dist)) %>% igraph::graph_from_adjacency_matrix() %>% igraph::components() %>% .$membership %>% as.integer()

nodes <- nodes_premerge %>%
  group_by(node_group_id) %>%
  mutate(x = mean(x), y = mean(y)) %>%
  select(-node_id) %>%
  rename(node_id = node_group_id)

if(!all(run$spaces$box_id %in% nodes$box_id)) {
  stop("Not all boxes have lines")
}



### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Map nodes on space                                                      ####
# now create cluster networks and cluster centers
cluster_networks <- nodes %>%
  group_by(path_id) %>%
  summarise(
    cluster_network = list(drop_na(tibble(from = lag(node_id, 1), to = node_id) %>% filter(from != to))),
    box_id = as.integer(first(box_id))
  ) %>%
  group_by(box_id) %>%
  summarise(cluster_network = list(bind_rows(cluster_network)))


global_x_scale <- 1.15
global_y_scale <- 1.15
cluster_positions <- nodes %>%
  group_by(node_id) %>%
  summarise(
    x = first(x),
    y = first(y),
    box_id = as.integer(first(box_id))
  ) %>%
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
  left_join(run$spaces %>% select(x_scale, y_scale, y_shift, x_shift, box_id), by = "box_id") %>%
  mutate(
    x = x * x_scale + x_shift,
    y = y * y_scale + y_shift
  ) %>%
  group_by(box_id) %>%
  summarise(cluster_positions = list(tibble(x = x, y = y, node_id = node_id))) %>%
  ungroup()

# make sure points overlap with data
run$spaces[100:120, ] %>% unnest(space) %>%
  ggplot(aes(Comp1, Comp2)) +
  geom_point() +
  geom_point(aes(x, y), color = "red", data = cluster_positions[100:120, ] %>% unnest(cluster_positions)) +
  facet_wrap(~box_id, scale = "free")


### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Project cells                                                           ####
predictions_data <- left_join(cluster_positions, cluster_networks, "box_id") %>% left_join(run$spaces, "box_id")
predictions_list <- predictions_data %>% pmap(function(box_id, cluster_positions, cluster_network, space, ...) {
  print(box_id)
  cluster_space <- cluster_positions %>% column_to_rownames("node_id") %>% as.matrix()
  cluster_network <- cluster_network %>% mutate_all(as.character) %>% mutate(directed = F, length = 1)
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
predictions <- tibble(prediction = predictions_list, dataset_id = run$spaces$id)



### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Check scores of controls                                                ####
datasets <- read_rds(derived_file("datasets.rds", "01-datasets/05-dataset_characterisation"))
datasets <- datasets %>% slice(match(run$spaces$id, id))

controls <- map(datasets %>% filter(dataset_group == "control" & id != "control_BA") %>% pull(id), function(dataset_id) {
  print(dataset_id)
  dataset <- extract_row_to_list(datasets, which(datasets$id == dataset_id))
  dataset$geodesic_dist <- dynutils::compute_tented_geodesic_distances(dataset)

  prediction <- extract_row_to_list(predictions, which(predictions$dataset_id == dataset_id))$prediction
  prediction$geodesic_dist <- dynutils::compute_tented_geodesic_distances(prediction)

  plot = dynplot::plot_default(prediction)
  plot

  tibble(
    scores = dyneval::calculate_metrics(dataset, prediction, c("correlation", "edge_flip", "rf_mse"))$summary %>%
      select(correlation, edge_flip) %>% list(),
    plot = list(plot),
    dataset_id = dataset_id
  ) %>% unnest(scores)
}) %>% bind_rows()

controls


##  ............................................................................
##  Save                                                                    ####
predictions %>% write_rds(derived_file(stringr::str_glue("predictions_{run$run_id}.rds")))

# upload to PRISM
qsub:::rsync_remote(
  remote_dest = "prism",
  path_dest = derived_file() %>% gsub(dynbenchmark::get_dynbenchmark_folder(), qsub:::run_remote("echo $DYNBENCHMARK_PATH", "prism")$cmd_out, .),
  remote_src = "",
  path_src = derived_file()
)
