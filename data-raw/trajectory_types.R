library(tidyverse)
library(colorspace)

################################
## Construct trajectory_types ##
################################

lighten <- function(color, factor=1.4){
  purrr::map_chr(color, function(color) {
    col <- col2rgb(color)
    col <- do.call(rgb2hsv, as.list(col))
    col[1] <- col[1] * 360
    col[2] <- 0.3
    col[3] <- 0.9
    colorspace::hex(do.call(colorspace::HSV, as.list(col)))
  })
}

trajectory_types <- tribble(
  ~id,                              ~simplified,            ~colour,   ~directed,

  # linear
  "undirected_linear",              "linear",               "#0278dd", FALSE,
  "directed_linear",                "linear",               "#0278dd", TRUE,

  # simple forks
  "simple_fork",                    "bifurcation",          "#3ad1d1", FALSE,
  "bifurcation",                    "bifurcation",          "#3ad1d1", TRUE,
  "convergence",                    "convergence",          "#1f8888", TRUE,

  # complex forks
  "complex_fork",                   "multifurcation",       "#7fbe00", FALSE,
  "multifurcation",                 "multifurcation",       "#7fbe00", TRUE,

  # binary trees
  "unrooted_binary_tree",           "binary",               "#00b009", FALSE,
  "rooted_binary_tree",             "binary",               "#00b009", TRUE,

  # trees
  "unrooted_tree",                  "tree",                 "#e0ab00", FALSE,
  "rooted_tree",                    "tree",                 "#e0ab00", TRUE,

  # cycles
  "undirected_cycle",               "cycle",                "#003d76", FALSE,
  "directed_cycle",                 "cycle",                "#003d76", TRUE,

  # dag
  "directed_acyclic_graph",         "acyclic_graph",        "#ff8821", TRUE,

  # graph
  "undirected_graph",               "graph",                "#ff4237", FALSE,
  "directed_graph",                 "graph",                "#ff4237", TRUE,

  # disconnected
  "disconnected_undirected_graph",  "disconnected_graph",   "#ca0565", TRUE,
  "disconnected_directed_graph",    "disconnected_graph",   "#ca0565", FALSE,

  # unknown
  "unknown",                        "unknown",              "#AAAAAA", NA
) %>% mutate(
  background_colour = lighten(colour, 0.3),
  directedness = ifelse(is.na(directed), "unknown", ifelse(directed, "directed", "undirected"))
)

###################################
## Construct trajectory_type_dag ##
###################################

trajectory_type_dag <- bind_rows(
  tribble(
    ~from, ~to,
    "directed_linear", "undirected_linear",
    "bifurcation", "simple_fork",
    "convergence", "simple_fork",
    "multifurcation", "complex_fork",
    "rooted_binary_tree", "unrooted_binary_tree",
    "rooted_tree", "unrooted_tree",
    "directed_cycle", "undirected_cycle",
    "directed_graph", "undirected_graph",
    "directed_acyclic_graph", "undirected_graph",
    "disconnected_directed_graph", "disconnected_undirected_graph",
    "unknown", "unknown"
  ) %>%
    mutate(prop_changes = "undirected") %>%
    mutate(prop_changes = as.list(prop_changes)),
  tribble(
    ~to, ~from, ~prop_changes,
    "simple_fork", "undirected_linear", "num_branch_nodes == 0",
    "complex_fork", "simple_fork", "max_degree == 3",
    "unrooted_binary_tree", "simple_fork", "num_branch_nodes == 1",
    "unrooted_tree", "complex_fork", "num_branch_nodes == 1",
    "unrooted_tree", "unrooted_binary_tree", "max_degree == 3",
    "undirected_graph", "unrooted_tree", "num_cycles == 0",
    "disconnected_undirected_graph", "undirected_graph", "num_components == 1",
    "undirected_graph", "undirected_cycle", c("num_branch_nodes == 0", "num_cycles == 1")
  ) %>% mutate(prop_changes = as.list(prop_changes)),
  tribble(
    ~to, ~from, ~prop_changes,
    "bifurcation", "directed_linear", "num_branch_nodes == 0",
    "bifurcation", "convergence", "",
    "convergence", "bifurcation", "",
    "convergence", "directed_linear", "num_branch_nodes == 0",
    "multifurcation", "bifurcation", "max_degree == 3",
    "rooted_binary_tree", "bifurcation", "num_branch_nodes == 1",
    "rooted_tree", "multifurcation", "num_branch_nodes == 1",
    "rooted_tree", "rooted_binary_tree", "max_degree == 3",
    "directed_acyclic_graph", "rooted_tree", "max_indegree == 1",
    "directed_acyclic_graph", "convergence", c("max_branch_nodes == 1", "max_out_degree == 1", "max_degree == 3"),
    "directed_graph", "directed_acyclic_graph", "num_cycles == 0",
    "disconnected_directed_graph", "directed_graph", "num_components == 1",
    "directed_graph", "directed_cycle", c("num_branch_nodes == 0", "num_cycles == 1")
  )
) %>%
  igraph::graph_from_data_frame(vertices = trajectory_types) %>%
  tidygraph::as_tbl_graph()

trajectory_type_ancestors <- trajectory_type_dag %>%
  igraph::ego(99999999, mode = "out") %>%
  map(names) %>%
  setNames(names(igraph::V(trajectory_type_dag)))

trajectory_types$ancestors <- trajectory_type_ancestors[trajectory_types$id]

###########################################
## Construct trajectory_types_simplified ##
###########################################

trajectory_types_simplified <- trajectory_types %>% select(simplified, colour, background_colour) %>% unique()


######################
## Save all objects ##
######################

devtools::use_data(trajectory_types, trajectory_type_dag, trajectory_types_simplified, overwrite = TRUE)



