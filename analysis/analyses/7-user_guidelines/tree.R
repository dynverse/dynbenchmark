library(cowplot)
library(tidyverse)
library(dynalysis)

library(tidygraph)
library(ggraph)

experiment("7-user_guidelines")

methods <- read_rds(derived_file("methods.rds", "4-method_characterisation"))
outputs_list <- read_rds(derived_file("outputs_postprocessed.rds", "5-optimise_parameters/3-evaluate_parameters"))
trajtype_scores <- outputs_list$outputs_summtrajtype_totals %>% filter(task_source == "mean") %>% rename(method_id = method_short_name)

decision_nodes_decisions <- tribble(
  ~node_id, ~node_label, ~type,
  "disconnected", "Do you expect multiple trajectories in the data?", "decision",
  "disconnected_yes", "Yes: Multiple trajectories could be contained in the data", "decision",
  "disconnected_directed_graph*", "≤ Disconnected directed graph", "topology",
  "disconnected_no", "No: The data contains one trajectory", "decision",
  "fixes_topology", "Do you expect a certain topology in the trajectory?", "decision",
  "fixes_topology_yes", "Yes", "decision",
  "fixes_topology_some", "Kind of: \nI know some characteristics of the topology", "decision",
  "directed_linear", NA, "topology",
  "bifurcation", NA, "topology",
  "directed_cycle", NA, "topology",
  "multifurcation*", "> Bifurcating", "topology",
  "contains_cycles", "Will the topology contain\ncycles or convergences?", "decision",
  "contains_cycles_no", "No", "decision",
  "contains_cycles_yes", "Yes/not sure", "decision",
  "at_least_bifurcations", "Do you expect the trajectory to contain at least 2 bifurcations?", "decision",
  "at_least_bifurcations_no", "Not necessarily", "decision",
  "at_least_bifurcations_yes", "Yes", "decision",
  "rooted_tree*", "≤ Rooted tree", "topology",
  "rooted_tree", NA, "topology",
  "directed_graph*", "≤ Directed (acyclic) graph", "topology",
  "fixes_topology_no", "No: \nI want to let the algorithm decide on the topology", "decision"
) %>%
  mutate(node_label = ifelse(is.na(node_label), label_long(node_id), node_label)) %>%
  mutate(trajectory_type = gsub("(.*)\\*", "\\1", node_id))

decision_edges_decisions <- tribble(
  ~from, ~to,
  "disconnected", "disconnected_yes",
  "disconnected_yes", "disconnected_directed_graph*",
  "disconnected", "disconnected_no",
  "disconnected_no", "fixes_topology",
  "fixes_topology", "fixes_topology_yes",
  "fixes_topology_yes", "directed_linear",
  "fixes_topology_yes", "bifurcation",
  "fixes_topology_yes", "directed_cycle",
  "fixes_topology_yes", "multifurcation*",
  "fixes_topology", "fixes_topology_some",
  "fixes_topology", "fixes_topology_no",
  "fixes_topology_some", "contains_cycles",
  "contains_cycles", "contains_cycles_no",
  "contains_cycles_no", "at_least_bifurcations",
  "at_least_bifurcations", "at_least_bifurcations_yes",
  "at_least_bifurcations", "at_least_bifurcations_no",
  "at_least_bifurcations_no", "rooted_tree*",
  "at_least_bifurcations_yes", "rooted_tree",
  "contains_cycles", "contains_cycles_yes",
  "contains_cycles_yes", "directed_graph*",
  "fixes_topology_no", "directed_graph*"
) %>% mutate(type = "decision")

if(!all(decision_edges_decisions %>% select(from, to) %>% unlist() %>% {. %in% decision_nodes_decisions$node_id})) stop("Not all nodes are present in dataframe")


decision_tree <- tbl_graph(decision_nodes_decisions, decision_edges_decisions %>% mutate(from=match(from, decision_nodes_decisions$node_id), to=match(to, decision_nodes_decisions$node_id)))
layout <- create_layout(decision_tree, "tree")
layout %>% ggraph() +
  geom_edge_link() +
  geom_node_label(aes(label=label_wrap(node_label, 25), color=type)) +
  theme_graph()

##

# get top methods per decision leaf

# first do trajectory types
trajectory_type_ids <- c("directed_linear", "bifurcation", "directed_cycle", "rooted_tree", "directed_graph*", "rooted_tree*", "disconnected_directed_graph*")
decision_methods <- map(trajectory_type_ids, function(node_id) {
  trajectory_type <- node_id %>% gsub("(.*)\\*", "\\1", .)
  if (str_sub(node_id, -1) == "*") {
    trajectory_types_to_score <- trajectory_types$ancestors[[which(trajectory_types$id == trajectory_type)]]
  } else {
    trajectory_types_to_score <- trajectory_type
  }

  linear_methods <- methods %>% filter(.data[[trajectory_type]]) %>% pull(method_id)
  top_methods <- trajtype_scores %>%
    filter(method_id %in% linear_methods) %>%
    filter(trajectory_type %in% trajectory_types_to_score) %>%
    group_by(method_id) %>%
    summarise(harm_mean = mean(harm_mean)) %>%
    top_n(5, harm_mean) %>%
    select(method_id, harm_mean) %>%
    mutate(origin = node_id) %>%
    arrange(-harm_mean)

  # if no top methods or if top method has low score: add ?
  if(nrow(top_methods) == 0 | max(top_methods$harm_mean) < 0.2) {
    top_methods <- top_methods %>% add_row(method_id = "?", harm_mean = 0.2, origin=node_id)
  }

  top_methods <- top_methods %>%
    arrange(-harm_mean) %>%
    mutate(method_i = row_number() - 1)

  top_methods

}) %>% bind_rows() %>%
  rename(score=harm_mean)

# add other fixed topology
decision_methods <- decision_methods %>% add_row(method_id = "?", score = 1, origin="multifurcation*", method_i = 0)

# now sort the origin, to keep the same ordering of vertices (will be used later to layout the x in the same order)
decision_methods$origin <- factor(decision_methods$origin, levels=intersect(decision_nodes$node_id, decision_methods$origin))
decision_methods <- decision_methods %>% arrange(origin) %>% mutate(origin = as.character(origin))

# create unique node_id
decision_methods$node_id <- paste0(decision_methods$origin, "-", decision_methods$method_id)

# create edges between methods
decision_edges_methods <- decision_methods %>%
  group_by(origin) %>%
  arrange(-score) %>%
  mutate(
    from = c(origin[[1]], node_id[-length(method_id)]),
    to = node_id,
    type = c("to_method", rep("method", n()-1))
  )



##  ............................................................................
##  Create tree                                                             ####





## Create

decision_nodes <- bind_rows(
  decision_nodes_decisions,
    decision_methods %>%
      mutate(type = "method") %>%
      left_join(methods %>% select(method_id, method_name), "method_id") %>%
      mutate(node_label = method_name) %>%
      mutate(node_label = ifelse(is.na(node_label), label_long(method_id), node_label))
  )

decision_edges <- bind_rows(
    decision_edges_decisions,
    decision_edges_methods
  )


decision_nodes <- decision_nodes %>%
  mutate(
    i = row_number()
) %>% group_by(origin) %>%
  mutate(
    alpha = ifelse(
      type == "method",
      percent_rank(c(0, score))[-1],
      1
    )
  ) %>%
  ungroup()

decision_nodes <- decision_nodes %>%
  mutate(
    fill =
      recode(
        type,
        method = "black",
        decision = "white",
        topology = ifelse(
          trajectory_type %in% trajectory_types$id,
          trajectory_types$color[match(trajectory_type, trajectory_types$id)],
          "#472000"
        )
      )
    )
decision_edges <- decision_edges %>% mutate(
  from = decision_nodes$i[match(from, decision_nodes$node_id)],
  to = decision_nodes$i[match(to, decision_nodes$node_id)]
)
if(any(is.na(decision_edges %>% select(from, to)))) stop("Not every edge in nodes")


type_fills <- c(method = "#333333", decision = "white")
type_colors <- c(method = "white", decision = "black", topology="white")

# create tree
decision_tree <- tbl_graph(decision_nodes, decision_edges)

# create layout
layout <- create_layout(decision_tree, "tree")

leaves <- V(decision_tree)[degree(decision_tree, mode="out") == 0] %>% as.integer()
leaves2 <- map(as.integer(V(decision_tree)[degree(decision_tree, mode="in") == 2]), function(v) ego(decision_tree, 1, v, mode="in")[[1]][-c(1,2)]) %>% unlist()
leaves <- unique(c(leaves, leaves2))
# leaves <- leaves[order(layout$x[leaves])]
leave_cols <- set_names(seq_along(leaves), leaves)

layout$x <- layout$ggraph.index %>% map_dbl(function(i) {
  leaves <- intersection(ego(decision_tree, 999999, i, "out")[[1]], leaves)
  mean(leave_cols[as.character(leaves)])
})

layout$level[layout$type == "method"] <- -layout$method_i[layout$type == "method"] * 0.5
layout$level[layout$type == "topology"] <- 1
layout$level[layout$type == "decision"] <- layout$y[layout$type == "decision"] - min(layout$y[layout$type == "decision"]) + 2
#
# # change layout to level
layout$y <- ifelse(is.na(layout$level), layout$y, layout$level)

layout %>% ggraph() +
  geom_edge_diagonal(aes(alpha=type), data = get_edges("short")(layout) %>% filter(type != "to_method")) +
  geom_edge_link(aes(xend=x + (xend-x)/1.2, yend = y + (yend-y)/1.2, alpha=type), arrow=arrow(type="closed", length=unit(0.1, "inches")), data = get_edges("short")(layout) %>% filter(type == "to_method")) +
  geom_node_label(aes(label=label_wrap(node_label, 25), fill=fill, color=type, alpha=alpha)) +
  scale_color_manual(values=type_colors) +
  scale_fill_identity() +
  scale_alpha_identity() +
  scale_edge_alpha_manual(values=c(method=0, decision=1, to_method=1)) +
  theme_graph() +
  scale_x_continuous(expand=c(0.2,0.2)) +
  theme(legend.position="none", plot.margin=margin(0, 0, 0, 0))

