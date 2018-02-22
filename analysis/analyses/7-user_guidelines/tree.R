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
  "fixes_topology", "Do you know the topology of the trajectory?", "decision",
  "fixes_topology_yes", "Yes: the topology is known", "decision",
  "fixes_topology_some", "I know some characteristics of the topology", "decision",
  "fixes_topology_no", "No: I want to let the algorithm decide on the topology", "decision",
  "directed_linear", NA, "topology",
  "bifurcation", NA, "topology",
  "directed_cycle", NA, "topology",
  "other_fixed_topology", "Other topology", "topology",
  "contains_cycles", "Will the topology contain\ncycles or convergences?", "decision",
  "contains_cycles_no", "No", "decision",
  "contains_cycles_yes", "Yes/not sure", "decision",
  "at_least_bifurcations", "Do you expect the trajectory to contain at least 2 bifurcations?", "decision",
  "at_least_bifurcations_no", "Not necessarily", "decision",
  "at_least_bifurcations_yes", "Yes", "decision",
  "rooted_tree*", "<= Rooted tree", "topology",
  "rooted_tree", "Rooted tree", "topology",
  "directed_graph*", "<= Directed (acyclic) graph", "topology"
) %>%
  mutate(node_label = ifelse(is.na(node_label), label_long(node_id), node_label))

decision_edges_decisions <- tribble(
  ~from, ~to,
  "fixes_topology", "fixes_topology_yes",
  "fixes_topology", "fixes_topology_some",
  "fixes_topology", "fixes_topology_no",
  "fixes_topology_yes", "directed_linear",
  "fixes_topology_yes", "bifurcation",
  "fixes_topology_yes", "directed_cycle",
  "fixes_topology_yes", "other_fixed_topology",
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
)

if(!all(decision_edges_decisions %>% unlist() %>% {. %in% decision_nodes_decisions$node_id})) stop("Not all nodes are present in dataframe")


decision_tree <- tbl_graph(decision_nodes_decisions, decision_edges_decisions)
layout <- create_layout(decision_tree, "tree")
layout %>% ggraph() +
  geom_edge_link() +
  geom_node_label(aes(label=label_wrap(node_label, 25), color=type)) +
  theme_graph()

##


decision_methods <- tribble(
  ~origin, ~method_id, ~score,
  "directed_linear", "scorpius", 1,
  "directed_linear", "embeddr", 0.9,
  "directed_linear", "waterfall", 0.8,
  "bifurcation", "slngsht", 1,
  "bifurcation", "tscan", 0.9,
  "bifurcation", "wishbone", 0.8,
  "bifurcation", "dpt", 0.7,
  "directed_cycle", "recat", 1,
  "other_fixed_topology", "?", 1,
  "rooted_tree", "mnclddr", 1,
  "rooted_tree", "slngsht", 0.9,
  "non_cycle_topology", "slngsht", 1,
  "directed_graph", "?", 1,
)

# get top methods per decision leaf

# first do trajectory types
trajectory_type_ids <- c("directed_linear", "bifurcation", "directed_cycle", "rooted_tree", "directed_graph*", "rooted_tree*")
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

  if(nrow(top_methods) == 0 | max(top_methods$harm_mean) < 0.2) {top_methods <- top_methods %>% add_row(method_id = "?", harm_mean = 0.2, origin=node_id)}

  top_methods

}) %>% bind_rows() %>%
  rename(score=harm_mean)

decision_methods <- decision_methods %>% add_row(method_id = "?", score = 1, origin="other_fixed_topology")





decision_methods$node_id <- paste0(decision_methods$origin, "-", decision_methods$method_id)

decision_edges_methods <- decision_methods %>%
  group_by(origin) %>%
  arrange(-score) %>%
  mutate(
    from = c(origin[[1]], node_id[-length(method_id)]),
    to = node_id
  )



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
          node_id %in% trajectory_types$id,
          trajectory_types$color[match(node_id, trajectory_types$id)],
          "grey"
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

decision_tree <- tbl_graph(decision_nodes, decision_edges)
layout <- create_layout(decision_tree, "tree")
layout %>% ggraph() +
  geom_edge_link() +
  geom_node_label(aes(label=label_wrap(node_label, 25), fill=fill, color=type, alpha=alpha)) +
  scale_color_manual(values=type_colors) +
  scale_fill_identity() +
  scale_alpha_identity() +
  theme_graph()

