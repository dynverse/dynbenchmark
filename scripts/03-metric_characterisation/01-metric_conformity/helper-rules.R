approx_unique <- function(x, tolerance = .Machine$double.eps ^ 0.5) {
  abs(first(sort(x)) - last(sort(x))) < tolerance
}

approx_equal <- function(x, y, tolerance = .Machine$double.eps ^ 0.5) {
  abs(x - y) < tolerance
}

# is the value of y monotonically increasing/decreasing with increasing x?
is_monotonic <- function(x, y, decreasing = TRUE) {
  if (decreasing) {
    all(diff(y[order(x)]) <= 0)
  } else {
    all(diff(y[order(x)]) >= 0)
  }
}

# is the value of y monotonic with x AND changing from the smallest x to the largest x?
is_monotonic_changing <- function(x, y, decreasing = TRUE) {
  is_monotonic(x, y, decreasing) && (
    if (decreasing) {
      y[which.min(x)] > y[which.max(x)]
    } else {
      y[which.min(x)] < y[which.max(x)]
    }
  )
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

      grouping <- dynwrap::group_onto_trajectory_edges(models$model[[1]])
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

switch_cells_edgewise <- rule_lower(
  id = "switch_cells_edgewise",
  description = "Switching the positions of cells within each edge should lower the score",
  dataset_ids = dataset_design %>% filter(topology_id %in% c("linear", "bifurcation")) %>% pull(dataset_id),
  method_id = "switch_cells_edgewise"
)

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
      summarise(conforms = is_monotonic_changing(!!varied_parameter_sym, mean_score))

    # plot the scores by smoothing
    plot_scores <- scores %>%
      ggplot(aes(!!varied_parameter_sym, score)) +
      geom_line(aes(group = dataset_id), alpha = 0.5, color = "black") +
      geom_smooth(color = "red") +
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
    grouping <- group_onto_trajectory_edges(models$model[[1]])
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

filter_cells <- rule_monotonic(
  id = "filter_cells",
  description = "Removing cells from the trajectory should lower the score smoothly",
  dataset_ids = dataset_design %>% filter(topology_id %in% c("linear")) %>% pull(dataset_id),
  method_id = "filter_cells",
  parameters = list(filter_cells = tibble(filter_perc = seq(0, 1, 0.1)) %>% mutate(id = as.character(filter_perc*10))),
  varied_parameter_id = "filter_perc"
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
  parameters = list(move_cells_subedges = tibble(n_edges = seq(0, 6), subedge_length_magnification = 1) %>% mutate(id = as.character(n_edges))),
  varied_parameter_id = "n_edges"
)

move_cells_subedges_magnified <- rule_monotonic(
  id = "move_cells_subedges_magnified",
  description = "The longer an extra edges, the lower the score",
  dataset_ids = dataset_design %>% filter(topology_id %in% c("linear", "bifurcation")) %>% pull(dataset_id),
  method_id = "move_cells_subedges",
  parameters = list(move_cells_subedges = tibble(subedge_length_magnification = seq(0,1, 0.2), n_edges = 1) %>% mutate(id = paste0("magnified_", as.character(subedge_length_magnification)))),
  varied_parameter_id = "subedge_length_magnification"
)

add_leaf_edges <- rule_monotonic(
  id = "add_leaf_edges",
  description = "Adding extra edges only connected to one existing milestone should lower the score",
  dataset_ids = dataset_design %>% filter(topology_id %in% c("linear", "bifurcation")) %>% pull(dataset_id),
  parameters = list(add_leaf_edges = tibble(n_edges = seq(0, 6)) %>% mutate(id = as.character(n_edges))),
  varied_parameter_id = "n_edges",
  method_id = "add_leaf_edges"
)

add_connecting_edges <- rule_monotonic(
  id = "add_connecting_edges",
  description = "Adding new edges between existing milestones should lower the score",
  dataset_ids = dataset_design %>% filter(topology_id %in% c("linear", "bifurcation")) %>% pull(dataset_id),
  parameters = list(add_connecting_edges = tibble(n_edges = seq(0, 4)) %>% mutate(id = as.character(n_edges))),
  varied_parameter_id = "n_edges",
  method_id = "add_connecting_edges"
)

time_warping_start <- rule_monotonic(
  id = "time_warping_start",
  description = "Warping the positions of the cells within each edge should lower the score",
  dataset_ids = dataset_design %>% filter(topology_id %in% c("linear", "bifurcation")) %>% pull(dataset_id),
  parameters = list(time_warping_start = tibble(warp_magnitude = seq(0, 6)) %>% mutate(id = as.character(warp_magnitude))),
  varied_parameter_id = "warp_magnitude",
  method_id = "time_warping_start"
)

time_warping_parabole <- rule_monotonic(
  id = "time_warping_parabole",
  description = "Warping the positions of the cells within each edge should lower the score",
  dataset_ids = dataset_design %>% filter(topology_id %in% c("linear", "bifurcation")) %>% pull(dataset_id),
  parameters = list(time_warping_parabole = tibble(warp_magnitude = seq(1, 20, 2)) %>% mutate(id = as.character(warp_magnitude))),
  varied_parameter_id = "warp_magnitude",
  method_id = "time_warping_parabole"
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
      left_join(dataset_design, c("dataset_id")) %>%
      mutate(
        topology_id = factor(topology_id, levels = unique(dataset_design$topology_id)),
        param_id = factor(param_id, levels = levels(topology_id))
      )

    # calculate scores when topologies stay the same
    top_scores <- scores %>%
      filter(param_id == topology_id) %>%
      select(dataset_id, metric_id, top_score = score)

    # check that when the topology has changed, the score decreased
    scores <- scores %>%
      filter(param_id != topology_id) %>%
      left_join(top_scores, c("dataset_id", "metric_id")) %>%
      mutate(conforms = score < top_score)

    conformity <- scores %>%
      group_by(metric_id) %>%
      summarise(conforms = all(conforms))

    # plot the scores for every metric
    plot_scores <-
      split(scores, scores$metric_id) %>% map(function(scores) {
        ggplot(scores, aes(topology_id, param_id)) +
          geom_tile(aes(fill = score)) +
          geom_text(aes(label = ifelse(conforms, "", "x"))) +
          scale_fill_viridis_c() +
          ggtitle(scores$metric_id)
      }) %>% patchwork::wrap_plots()

    # plot every topology
    models <- models %>%
      group_by(param_id) %>%
      filter(row_number() == 1) %>%
      ungroup() %>%
      mutate(param_id = factor(param_id, levels = levels(scores$topology_id)))

    plot_datasets <- map2(models$param_id, models$model, function(title, model) {
      dynplot::plot_topology(model) + ggtitle(title)
    }) %>% patchwork::wrap_plots()

    lst(
      conformity,
      plot_scores,
      plot_datasets
    )
  }
)



switch_cells_grouped <- lst(
  id = "switch_cells_grouped",
  description = "Changing the position of the cells in a continuous or grouped dataset should lower the score similarly",
  parameters = c(switch_cells$parameters, list(switch_cells_grouped = switch_cells$parameters$switch_cells)),
  crossing = bind_rows(switch_cells$crossing, switch_cells$crossing %>% mutate(method_id = "switch_cells_grouped")),
  assessment = function(scores, rule, models) {
    # check if scores are highly correlated
    conformity <- scores %>%
      spread(method_id, score) %>%
      group_by(metric_id) %>%
      summarise(cor = cor(switch_cells, switch_cells_grouped)) %>%
      mutate(conforms = ifelse(is.na(cor), TRUE, cor > 0.8))

    # plot the relation between the scores on the continuous and grouped datasets
    plot_scores <- scores %>%
      spread(method_id, score) %>%
      ggplot(aes(switch_cells, switch_cells_grouped)) +
        geom_point() +
        geom_abline(slope = 1, intercept = 0, color = "red") +
        geom_smooth(se = FALSE, method = lm) +
        facet_wrap(~metric_id, scales = "free")

    # plot a continuous and grouped model
    models <- models %>%
      left_join(dataset_design, "dataset_id") %>%
      filter(param_id == 0, num_cells == 100) %>%
      group_by(method_id) %>%
      slice(1)

    plot_datasets <- map2(
      models$model,
      models$method_id,
      function(model, title) {
        plot_graph(model, color_cells = "milestone") + ggtitle(label_long(title)) + theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
      }) %>%
      patchwork::wrap_plots()

    lst(
      conformity,
      plot_scores,
      plot_datasets
    )
  }
)





combined_position_topology <- lst(
  id = "combined_position_topology",
  description = "Changing both the topology and the cell positions should lower the score more than any of the two individually",
  parameters = list(
    identity = tibble(id = "default"),
    switch_cells = tibble(switch_perc = 0.2, id = "combination"),
    add_connecting_edges = tibble(n_edges = 1, id = "combination"),
    switch_cells_and_add_connecting_edges = tibble(switch_perc = 0.2, n_edges = 1, id = "combination")
  ),
  crossing =  map2_df(
    names(parameters),
    parameters,
    function(method_id, parameters) {
      crossing(
        method_id,
        param_id = parameters$id,
        dataset_id = dataset_design %>% filter(num_cells == 100) %>% filter(topology_id %in% names(topologies)) %>% pull(dataset_id)
      )
    }
  ),
  assessment = function(scores, rule, models) {
    scores <- scores %>%
      mutate(
        method_id = factor(method_id, levels = c("identity", "switch_cells", "add_connecting_edges", "switch_cells_and_add_connecting_edges"))
      )

    # check if scores are highly correlated
    conformity <- scores %>%
      select(-param_id) %>%
      spread(method_id, score) %>%
      group_by(metric_id) %>%
      summarise(
        conforms =
          (mean(switch_cells_and_add_connecting_edges) < mean(switch_cells)) &&
          (mean(switch_cells_and_add_connecting_edges) < mean(add_connecting_edges))
      )

    # plot the scores on only topology, position and combined
    plot_scores <- scores %>%
      ggplot(aes(method_id, score, fill = method_id)) +
      geom_boxplot() +
      facet_wrap(~metric_id, scales = "free_y") +
      scale_fill_manual(values = c(
        identity = "grey",
        switch_cells = "#0074D9",
        add_connecting_edges = "#FF4136",
        switch_cells_and_add_connecting_edges = "#85144b"
      ))

    # plot the four models
    models <- models %>%
      mutate(method_id = factor(method_id, levels = levels(scores$method_id))) %>%
      left_join(dataset_design, "dataset_id") %>%
      filter(num_cells == 100) %>%
      group_by(method_id) %>%
      slice(1) %>%
      arrange(method_id)

    grouping <- group_onto_trajectory_edges(models$model[[1]])

    plot_datasets <- map2(
      models$model,
      models$method_id,
      function(model, title) {
        plot_graph(model, grouping = grouping) + ggtitle(label_long(title)) + theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
      }) %>%
      patchwork::wrap_plots()

    lst(
      conformity,
      plot_scores,
      plot_datasets
    )
  }
)


# combine rules
rules <- list(
  equal_identity,
  switch_cells_edgewise,
  merge_bifurcation,
  concatenate_bifurcation,
  break_cycle,
  join_linear,
  switch_cells,
  filter_cells,
  move_cells_subedges,
  move_cells_subedges_magnified,
  add_leaf_edges,
  add_connecting_edges,
  switch_edges,
  time_warping_start,
  time_warping_parabole,
  shuffle_lengths,
  change_topology,
  switch_cells_grouped,
  combined_position_topology
) %>% map(~list(.) %>% list_as_tibble()) %>% bind_rows()

# rules <- rules %>% filter(id %in% c("time_warping_parabole"))

# check rules for contents
walkdf(rules, function(rule) {
  testthat::expect_true(all(
    c("id", "description", "crossing", "assessment") %in% names(rule)
  ))
})
