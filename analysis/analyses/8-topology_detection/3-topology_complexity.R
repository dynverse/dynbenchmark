library(cowplot)
library(tidyverse)
library(dynalysis)

library(tidygraph)
library(ggraph)

experiment("8-compare_topology")

#   ____________________________________________________________________________
#   Preparation                                                             ####
label_facet_methods <- function(x) {tibble(method_id=methods$method_name[match(x$method_id, methods$method_id)])}

# functions to calculate statistics of milestone network
calculate_n_edges <- function(milestone_network) {
  milestone_network %>%
    nrow()
}
calculate_n_milestones <- function(milestone_network) {
  milestone_network %>%
    select(from, to) %>%
    unlist() %>%
    unique() %>%
    length()
}
calculate_statistics <- function(milestone_network) {
  if (!is.null(milestone_network)) {
    milestone_network <- milestone_network %>%
      filter(to != "FILTERED_CELLS") %>%
      igraph::graph_from_data_frame() %>%
      dynutils::simplify_igraph_network() %>%
      igraph::as_data_frame()
    tibble(
      n_edges = calculate_n_edges(milestone_network),
      n_milestones = calculate_n_milestones(milestone_network),
      complexity = n_edges + n_milestones
    )
  } else {
    tibble(
      n_edges = NA,
      n_milestones = NA,
      complexity = NA
    )
  }

}

# determine top methods
methods <- read_rds(derived_file("methods.rds", experiment_id = "4-method_characterisation"))
outputs_list <- read_rds(derived_file("outputs_postprocessed.rds", "5-optimise_parameters/3-evaluate_parameters"))
method_order <- outputs_list$outputs_summmethod_totals %>%
  rename(method_id = method_short_name) %>%
  filter(task_source=="mean") %>%
  arrange(-harm_mean) %>%
  filter(method_id %in% methods$method_id) %>%
  pull(method_id)

# load config and tasks
config <- read_rds(derived_file("config.rds", "5-optimise_parameters/3-evaluate_parameters"))
tasks <- read_rds(derived_file("tasks.rds", "2-dataset_characterisation")) %>% mutate(task_id = id)
tasks$trajectory_type <- map(tasks$milestone_network, dynutils::classify_milestone_network) %>% map_chr("network_type")
tasks <- bind_cols(tasks, map_df(tasks$milestone_network, calculate_statistics))

filter_errored_trajectory_types <- function(x) {
  map_chr(x, ~ifelse(is.null(.), "unknown", .))
}
trajectory_types_simplified_order <- intersect(trajectory_types_simplified$simplified, simplify_trajectory_type(tasks %>% filter(task_id %in% config$task_ids) %>% pull(trajectory_type)))


#   ____________________________________________________________________________
#   Load models & generate statistics                                       ####
milestone_networks <- pbapply::pblapply(cl=4, method_order[1:5], function(method_id) {
  print(method_id)
  output_models <- read_rds(derived_file(paste0("suite/", method_id, "/output_models.rds"), "5-optimise_parameters/3-evaluate_parameters")) %>%  list_as_tibble()
  output_metrics <- read_rds(derived_file(paste0("suite/", method_id, "/output_metrics.rds"), "5-optimise_parameters/3-evaluate_parameters"))

  output_models <- bind_cols(output_models, map_df(output_models$milestone_network, calculate_statistics))

  tasks <- tasks %>% filter(task_id %in% output_metrics$task_id)

  milestone_networks <- output_models %>%
    select(milestone_network, trajectory_type, n_edges, n_milestones, complexity) %>%
    rename_all(~paste0(., "_predicted")) %>%
    mutate(task_id = output_metrics$task_id) %>%
    left_join(
      tasks %>% select(task_id, milestone_network, trajectory_type, n_edges, n_milestones, complexity) %>% rename_at(vars(-task_id), ~paste0(., "_gold")),
      "task_id"
    ) %>%
    mutate(trajectory_type_predicted = filter_errored_trajectory_types(trajectory_type_predicted)) %>%
    mutate(method_id = method_id)
}) %>%
  bind_rows() %>%
  mutate_at(vars(trajectory_type_predicted, trajectory_type_gold), simplify_trajectory_type) %>%
  mutate_at(vars(trajectory_type_predicted, trajectory_type_gold), factor, trajectory_types_simplified_order) %>%
  mutate(method_id = forcats::fct_inorder(method_id))



#   ____________________________________________________________________________
#   Plotting                                                                ####

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Compare trajectory types                                                ####
trajectory_type_comparison <- milestone_networks %>%
  group_by(trajectory_type_gold, trajectory_type_predicted, method_id) %>%
  count() %>%
  ungroup() %>%
  complete(trajectory_type_gold, trajectory_type_predicted, method_id, fill=list(n=0)) %>%
  group_by(trajectory_type_gold, method_id) %>%
  mutate(n=n/sum(n)) %>%
  ggplot(aes(trajectory_type_predicted, trajectory_type_gold)) +
  geom_tile(aes(fill=n)) +
  geom_tile(fill=NA, color="white", size=1) +
  geom_tile(aes(simplified, simplified, color=color), fill=NA, data=trajectory_types_simplified %>% filter(simplified %in% levels(milestone_networks$trajectory_type_predicted)), size=1) +
  scale_x_discrete(label_long("trajectory_type_predicted"), label=label_long) +
  scale_y_discrete(label_long("trajectory_type_gold"), label=label_long) +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  scale_color_identity() +
  scale_fill_distiller("% datasets", limits=c(0, 1), direction = 1, palette="Greys", label=scales::percent, breaks=c(0, 0.5, 1)) +
  #coord_equal() +
  facet_grid(.~method_id, labeller=label_facet_methods) +
  theme(legend.position="top")

trajectory_type_comparison


### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Compare complexity distributions                                        ####

bw <- 1.5
arrow_y <- 10
trajectory_type_colors <- c(set_names(trajectory_types_simplified$color, trajectory_types_simplified$simplified), "all_trajectory_types"="#333333")
complexity_difference_limits <- with(milestone_networks, c(min(complexity_predicted - complexity_gold, na.rm=T)-bw, max(complexity_predicted - complexity_gold, na.rm=T)+bw))
complexity_difference_distribution <- milestone_networks %>%
  mutate(complexity_difference = complexity_predicted - complexity_gold) %>%
  {
    bind_rows(
    .,
    mutate(., trajectory_type_gold = "all_trajectory_types")
    ) %>% mutate(trajectory_type_gold = factor(trajectory_type_gold, levels=c(trajectory_types_simplified$simplified, "all_trajectory_types")))
  } %>%
  ggplot(aes(complexity_difference, y=trajectory_type_gold, fill=trajectory_type_gold)) +
  ggridges::geom_density_ridges2(
    stat="density_ridges",
    alpha=0.8,
    bandwidth=bw,
    from=complexity_difference_limits[[1]],
    to=complexity_difference_limits[[2]]
  ) +
  geom_vline(xintercept = 0, color="black", linetype = "dashed") +
  scale_fill_manual(values=trajectory_type_colors) +
  facet_grid(.~method_id, labeller=label_facet_methods) +
  scale_y_discrete(label_long("trajectory_type_gold"), expand = c(0,0), labels=label_long) +
  scale_x_continuous(label_long("Difference in topological complexity (= # nodes + # edges)\nbetween prediction and gold standard"), expand = c(0, 0), limits = complexity_difference_limits) +
  annotate("segment", x = complexity_difference_limits[[2]]/3, xend = complexity_difference_limits[[2]], y = arrow_y, yend = arrow_y, colour = "#333333", arrow=arrow(type="closed", length=unit(0.1, "inches"))) +
  annotate("text", x = complexity_difference_limits[[2]], y = arrow_y, colour = "#333333", label="More complex", hjust=1, vjust=1.5, lineheight = 0.8, size=3) +
  theme(legend.position = "none", axis.text.y=element_text(vjust=0))
complexity_difference_distribution



### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Common plot                                                             ####
topology_complexity_comparison <- plot_grid(
  trajectory_type_comparison,
  complexity_difference_distribution,
  ncol=1,
  align="v",
  axis="lr",
  labels="auto"
)

topology_complexity_comparison
topology_complexity_comparison %>% ggsave(figure_file("topology_complexity_comparison.svg"), ., width=12, height=8)
