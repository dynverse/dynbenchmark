approx_unique <- function(x, tolerance = .Machine$double.eps ^ 0.5) {
  abs(first(sort(x)) - last(sort(x))) < tolerance
}

approx_equal <- function(x, y, tolerance = .Machine$double.eps ^ 0.5) {
  abs(x - y) < tolerance
}

is_monotonic <- function(x, y, decreasing = TRUE) {
  if (decreasing) {
    all(diff(y[order(x)]) < 0)
  } else {
    all(diff(y[order(x)]) > 0)
  }
}

group_trajectory_edges <- function(trajectory, group_template = "{from}->{to}") {
  # first map cells to largest percentage (in case of divergence regions)
  progressions <- trajectory$progressions %>%
    group_by(cell_id) %>%
    arrange(-percentage) %>%
    slice(1) %>%
    ungroup()

  # do the actual grouping
  progressions %>%
    group_by(from, to) %>%
    mutate(group_id = as.character(glue::glue(group_template))) %>%
    ungroup() %>%
    select(cell_id, group_id) %>%
    deframe()
}

equal_identity <- lst(
  id = "equal_identity",
  description = "The score should always be the same when comparing the gold standard to itself",
  crossing = crossing(
    dataset_id = dataset_design %>% filter(topology_id == "bifurcation") %>% pull(dataset_id), ## TODO: add all datasets here
    method_id = "identity"
  ),
  assessment = function(scores, rule, models) {
    lst(
      conformity = scores %>%
        group_by(metric_id) %>%
        summarise(conforms = approx_unique(score))
      ,
      plot_scores = scores %>%
        ggplot(aes(metric_id, score)) +
        geom_boxplot()
      ,
      plot_datasets = models %>%
        left_join(dataset_design, "dataset_id") %>%
        filter(num_cells == 100) %>%
        pull(model) %>%
        first() %>%
        {plot_graph(.) + ggtitle("Identity")}
    )
  }
)


# create a rule which checks whether the score decreases compared to gold standard
rule_lower <- function(
  id,
  description,
  dataset_ids,
  method_id
) {
  lst(
    id = id,
    description = description,
    crossing = crossing(
      dataset_id = dataset_ids,
      method_id = c("identity", method_id)
    ),
    assessment = function(scores, rule, models) {
      # sort so that identity gets first
      scores <- scores %>%
        mutate(method_id = factor(method_id, levels = c("identity", !!method_id))) %>%
        arrange(method_id)

      # spread the scores, then check whether the "identity" column is higher than the method_id column
      conformity <- scores %>%
        spread(method_id, score) %>%
        group_by(metric_id) %>%
        summarise(conforms = all(!!rlang::sym(method_id) < identity))

      # plot the scores using a line graph
      plot_scores <- scores %>%
        ggplot(aes(method_id, score)) +
        geom_line(aes(group = dataset_id)) +
        geom_point() +
        facet_wrap(~metric_id, scales = "free_y")

      # plot the models
      models <- models %>%
        left_join(dataset_design, "dataset_id") %>%
        group_by(method_id) %>%
        filter(num_cells == 100) %>%
        slice(1) %>%
        ungroup() %>%
        arrange(factor(method_id, levels=levels(scores$method_id)))

      grouping <- group_trajectory_edges(models$model[[1]])
      plot_datasets <- map2(
        models$model,
        models$method_id,
        function(model, title) {
          plot_graph(model, grouping=grouping) + ggtitle(label_long(title)) + theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
        }) %>%
        patchwork::wrap_plots()

      lst(
        conformity,
        plot_scores,
        plot_datasets
      )
    }
  )
}

merge_bifurcation <- rule_lower(
  id = "merge_bifurcation",
  description = "Merging the two branches after a bifurcation should lower the score",
  dataset_ids = dataset_design %>% filter(topology_id == "bifurcation_simple") %>% pull(dataset_id),
  method_id = "merge_bifurcation"
)

concatenate_bifurcation <- rule_lower(
  id = "concatenate_bifurcation",
  description = "Concatenating one branch of a bifurcation to the other branch should lower the score",
  dataset_ids = dataset_design %>% filter(topology_id == "bifurcation_simple") %>% pull(dataset_id),
  method_id = "concatenate_bifurcation"
)

break_cycle <- rule_lower(
  id = "break_cycle",
  description = "Breaking a cyclic trajectory should lower the score",
  dataset_ids = dataset_design %>% filter(topology_id == "cycle") %>% pull(dataset_id),
  method_id = "break_cycle"
)

join_linear <- rule_lower(
  id = "join_linear",
  description = "Joining the two ends of a linear trajectory should lower the score",
  dataset_ids = dataset_design %>% filter(topology_id == "linear") %>% pull(dataset_id),
  method_id = "join_linear"
)

move_terminal_branch <- rule_lower(
  id = "move_leaf_branch",
  description = "Moving a leaf branch should lower the score",
  dataset_ids = dataset_design %>% filter(topology_id %in% c("linear", "tree")) %>% pull(dataset_id),
  method_id = "move_terminal_branch"
)

shuffle_lengths <- rule_lower(
  id = "shuffle_lengths",
  description = "Shuffling the lengths of the milestone network topology should lower the score",
  dataset_ids = dataset_design %>% filter(topology_id %in% c("linear", "bifurcation", "tree")) %>% pull(dataset_id),
  method_id = "shuffle_lengths"
)





rule_monotonic <- function(
  id,
  description,
  dataset_ids,
  method_id,
  parameters,
  varied_parameter_id
) {
  assessment <- function(scores, rule, models) {
    varied_parameter_sym <- rlang::sym(varied_parameter_id)

    # join scores with the parameters
    scores <- scores %>%
      left_join(parameters[[method_id]], by=c("param_id" = "id"))

    # check whether the score decreases monotonically
    conformity = scores %>%
      group_by(metric_id, !!varied_parameter_sym) %>%
      summarise(mean_score = mean(score)) %>%
      summarise(conforms = is_monotonic(!!varied_parameter_sym, mean_score))

    # plot the scores by smoothing
    plot_scores <- scores %>%
      ggplot(aes(!!varied_parameter_sym, score, colour = metric_id)) +
      geom_boxplot(aes(group = !!varied_parameter_sym)) +
      geom_smooth() +
      facet_wrap(~metric_id, scales = "free_y")

    # select models to plot
    models <- models %>%
      left_join(dataset_design, "dataset_id") %>%
      filter(num_cells == 100) %>%
      left_join(parameters[[method_id]], by=c("param_id" = "id")) %>%
      filter(!!varied_parameter_sym %in% c(min(!!varied_parameter_sym), sort(!!varied_parameter_sym)[round(n()/2)], max(!!varied_parameter_sym))) %>%
      group_by(!!varied_parameter_sym) %>%
      slice(1)

    # do the actual plotting
    grouping <- group_trajectory_edges(models$model[[1]])
    plot_datasets <- map2(
      models$model,
      glue::glue("{label_long(models$method_id)} ({varied_parameter_id}={models[[varied_parameter_id]]})"),
      function(model, title) {
        plot_graph(model, grouping=grouping) + ggtitle(label_long(title)) + theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
      }) %>%
      patchwork::wrap_plots()

    lst(
      conformity,
      plot_scores,
      plot_datasets
    )
  }

  lst(
    id,
    description,
    parameters,
    crossing = crossing(
      dataset_id = dataset_ids,
      method_id = method_id,
      param_id = parameters[[method_id]]$id
    ),
    assessment
  )
}


switch_cells <- rule_monotonic(
  id = "switch_cells",
  description = "Switching the positions of cells should lower the score smoothly",
  dataset_ids = dataset_design %>% filter(topology_id %in% c("linear", "bifurcation")) %>% pull(dataset_id),
  method_id = "switch_cells",
  parameters = list(switch_cells = tibble(switch_perc = seq(0, 1, 0.1)) %>% mutate(id = as.character(switch_perc*10))),
  varied_parameter_id = "switch_perc"
)

remove_cells <- rule_monotonic(
  id = "remove_cells",
  description = "Removing cells from the trajectory should lower the score smoothly",
  dataset_ids = dataset_design %>% filter(topology_id %in% c("linear")) %>% pull(dataset_id),
  method_id = "remove_cells",
  parameters = list(remove_cells = tibble(remove_perc = seq(0, 1, 0.1)) %>% mutate(id = as.character(remove_perc*10))),
  varied_parameter_id = "remove_perc"
)

switch_edges <- rule_monotonic(
  id = "switch_edges",
  description = "Switching the edges should lower the score smoothly",
  dataset_ids = dataset_design %>% filter(topology_id %in% c("tree")) %>% pull(dataset_id),
  method_id = "switch_edges",
  parameters = list(switch_edges = tibble(switch_perc = seq(0, 1, 0.25)) %>% mutate(id = as.character(switch_perc*10))),
  varied_parameter_id = "switch_perc"
)

move_cells_subedges <- rule_monotonic(
  id = "move_cells_subedges",
  description = "Moving some cells into short subedges should lower the score",
  dataset_ids = dataset_design %>% filter(topology_id %in% c("linear", "bifurcation")) %>% pull(dataset_id),
  method_id = "move_cells_subedges",
  parameters = list(move_cells_subedges = tibble(n_edges = seq(0, 6)) %>% mutate(id = as.character(n_edges))),
  varied_parameter_id = "n_edges"
)

move_cells_subedges_magnified <- rule_monotonic(
  id = "move_cells_subedges_magnified",
  description = "The longer an extra edges, the lower the score",
  dataset_ids = dataset_design %>% filter(topology_id %in% c("linear", "bifurcation")) %>% pull(dataset_id),
  method_id = "move_cells_subedges",
  parameters = list(move_cells_subedges = tibble(subedge_length_magnification = seq(0,1, 0.2)) %>% mutate(id = paste0("magnified_", as.character(subedge_length_magnification)))),
  varied_parameter_id = "subedge_length_magnification"
)

add_edges <- rule_monotonic(
  id = "add_edges",
  description = "Adding extra edges to the topology should lower the score",
  dataset_ids = dataset_design %>% filter(topology_id %in% c("linear", "bifurcation")) %>% pull(dataset_id),
  parameters = list(add_edges = tibble(n_edges = seq(0, 6)) %>% mutate(id = as.character(n_edges))),
  varied_parameter_id = "n_edges",
  method_id = "add_edges"
)

time_warping <- rule_monotonic(
  id = "time_warping",
  description = "Warping the positions of the cells within each edge should lower the score",
  dataset_ids = dataset_design %>% filter(topology_id %in% c("linear", "bifurcation")) %>% pull(dataset_id),
  parameters = list(time_warping = tibble(warp_magnitude = seq(0, 6)) %>% mutate(id = as.character(warp_magnitude))),
  varied_parameter_id = "warp_magnitude",
  method_id = "time_warping"
)

source(scripts_file("helper-topologies.R"))
change_topology <- lst(
  id = "change_topology",
  description = "A change in topology should lower the score",
  parameters = list(change_topology = tibble(topology_id = names(topologies), id = topology_id)),
  crossing = crossing(
    dataset_id = dataset_design %>% filter(num_cells == 100) %>% filter(topology_id %in% names(topologies)) %>% pull(dataset_id),
    method_id = "change_topology",
    param_id = parameters[[method_id]]$id
  ),
  assessment = function(scores, rule, models) {
    # join with dataset design to know the topology of the dataset
    scores <- scores %>%
      left_join(dataset_design, c("dataset_id"))

    # calculate scores when topologies stay the same
    top_scores <- scores %>%
      filter(param_id == topology_id) %>%
      select(dataset_id, metric_id, top_score = score)

    # check that when the topology has changed, the score decreased
    conformity <- scores %>%
      filter(param_id != topology_id) %>%
      left_join(top_scores, c("dataset_id", "metric_id")) %>%
      group_by(metric_id) %>%
      summarise(conforms = all(score < top_score))

    plot_scores <- ggplot(scores) +
      geom_tile(aes(topology_id, param_id, fill = score)) +
      facet_wrap(~metric_id)

    plot_datasets <- NULL

    lst(
      conformity,
      plot_scores,
      plot_datasets
    )
  }
)


# combine rules
rules <- list(
  # equal_identity,
  # merge_bifurcation,
  # concatenate_bifurcation,
  # break_cycle,
  # join_linear,
  # switch_cells,
  # remove_cells,
  # move_cells_subedges
  # move_cells_subedges_magnified
  # add_edges,
  # switch_edges,
  # time_warping,
  # shuffle_lengths,
  change_topology
) %>% map(~list(.) %>% list_as_tibble()) %>% bind_rows()

# check rules for contents
walkdf(rules, function(rule) {
  testthat::expect_true(all(
    c("id", "description", "crossing", "assessment") %in% names(rule)
  ))
})
