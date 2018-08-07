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
    metric_cutoffs <- dyneval::metrics %>%
      filter(metric_id %in% scores$metric_id) %>%
      mutate(cutoff = perfect - (perfect - worst) * 0.01) %>%
      select(metric_id, cutoff) %>%
      deframe()
    metric_cutoffs[setdiff(scores$metric_id, names(metric_cutoffs))] <- 0.99

    labels <- set_names(names(metric_cutoffs)) %>%
      map(label_metric, parse = TRUE) %>%
      map_chr(as.character) %>%
      parse(text = .)

    lst(
      conformity = scores %>%
        mutate(higher_or_equal = score >= metric_cutoffs[as.character(metric_id)]) %>%
        group_by(metric_id) %>%
        summarise(conforms = all(higher_or_equal))
      ,
      plot_scores = scores %>%
        ggplot(aes(metric_id, score)) +
        geom_boxplot() +
        geom_hline(aes(yintercept = cutoff), data = metric_cutoffs %>% enframe("metric_id", "cutoff"), linetype = "dashed") +
        scale_y_reverse() +
        scale_x_discrete(labels = labels) +
        coord_flip() +
        theme_bw() +
        theme(
          panel.grid.minor = element_blank(),
          panel.border = element_blank()
        )
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
        arrange(method_id) %>%
        mutate(metric_id = factor(metric_id))

      differences <- scores %>%
        spread(method_id, score) %>%
        mutate(difference = !!rlang::sym(method_id) - identity)

      # spread the scores, then check whether the "identity" column is higher than the method_id column
      conformity <- differences %>%
        group_by(metric_id) %>%
        summarise(conforms = all(difference < 0))

      # plot the scores using a dot plot
      differences_mean <- differences %>%
        group_by(metric_id) %>%
        summarise(difference = mean(difference))

      labels <- set_names(levels(differences$metric_id)) %>%
        map(label_metric, parse = TRUE) %>%
        map_chr(as.character) %>%
        parse(text = .)

      plot_scores <- differences %>%
        ggplot(aes(difference, metric_id)) +
        geom_point(shape = "|", size = 4) +
        geom_point(data = differences_mean, color = "red", size = 3) +
        geom_vline(xintercept = 0, linetype = "dashed") +
        scale_y_discrete(NULL, expand = c(0.1, 0), labels = labels) +
        scale_x_reverse("Score difference", limits = c(max(differences$difference, 0), -1)) +
        theme_bw() +
        theme(
          panel.grid.minor = element_blank(),
          panel.border = element_blank()
        )

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

split_linear <- rule_lower(
  id = "split_linear",
  description = "Splitting a linear trajectory into a bifurcation should lower the score",
  dataset_ids = dataset_design %>% filter(topology_id == "linear") %>% pull(dataset_id),
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
  varied_parameter_id,
  varied_parameter_name = varied_parameter_id,
  varied_parameter_labeller = identity
) {
  assessment <- function(scores, rule, models) {
    varied_parameter_sym <- rlang::sym(varied_parameter_id)

    # join scores with the parameters
    scores <- scores %>%
      left_join(parameters[[method_id]], by=c("param_id" = "id"))

    scores_mean <- scores %>%
      group_by(!!varied_parameter_sym, metric_id) %>%
      summarise(score = mean(score))

    # check whether the score decreases monotonically
    conformity = scores %>%
      group_by(metric_id, !!varied_parameter_sym) %>%
      summarise(mean_score = mean(score)) %>%
      summarise(conforms = is_monotonic_changing(!!varied_parameter_sym, mean_score))

    # plot the scores in a line graph
    plots <- scores %>%
      nest(-metric_id, .key = "scores") %>%
      mutate(metric_id = as.character(metric_id)) %>% # to avoid pmap converting metric_id to numeric
      pmap(function(metric_id, scores) {
        scores_mean <- scores %>%
          group_by(method_id, !!varied_parameter_sym) %>%
          summarise(score = mean(score)) %>%
          ungroup()

        limits <- sort(limits_metric(metric_id))
        breaks <- c(unname(limits_metric(metric_id)), round(last(scores_mean$score), 2))

        ggplot(scores, aes(!!varied_parameter_sym, score)) +
          geom_line(aes(group = dataset_id), alpha = 0.5, color = "black") +
          geom_line(aes(group = metric_id), data = scores_mean, color = "red", size = 2) +
          geom_hline(aes(yintercept = value), data = limits_metric(metric_id) %>% enframe(), linetype = "dashed") +
          scale_y_continuous(NULL, limits = limits, breaks = breaks) +
          scale_x_continuous(varied_parameter_name, expand = c(0.1, 0), labels = varied_parameter_labeller) +
          labs(
            title = label_metric(metric_id, parse = TRUE),
            parse = TRUE
          ) +
          theme_bw() +
          theme(
            panel.grid.minor = element_blank(),
            panel.border = element_blank()
          )
      })
    plot_scores <- patchwork::wrap_plots(plots, ncol = 4)

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
  varied_parameter_id = "switch_perc",
  varied_parameter_name = "Switched cells",
  varied_parameter_labeller = scales::percent
)

filter_cells <- rule_monotonic(
  id = "filter_cells",
  description = "Removing cells from the trajectory should lower the score smoothly",
  dataset_ids = dataset_design %>% filter(topology_id %in% c("linear")) %>% pull(dataset_id),
  method_id = "filter_cells",
  parameters = list(filter_cells = tibble(filter_perc = seq(0, 1, 0.1)) %>% mutate(id = as.character(filter_perc*10))),
  varied_parameter_id = "filter_perc",
  varied_parameter_name = "Filtered cells",
  varied_parameter_labeller = scales::percent
)

switch_edges <- rule_monotonic(
  id = "switch_edges",
  description = "Switching the edges should lower the score smoothly",
  dataset_ids = dataset_design %>% filter(topology_id %in% c("tree")) %>% pull(dataset_id),
  method_id = "switch_edges",
  parameters = list(switch_edges = tibble(switch_perc = seq(0, 1, 0.25)) %>% mutate(id = as.character(switch_perc*10))),
  varied_parameter_id = "switch_perc",
  varied_parameter_name = "Switched edges",
  varied_parameter_labeller = scales::percent
)

move_cells_subedges <- rule_monotonic(
  id = "move_cells_subedges",
  description = "Moving some cells into short subedges should lower the score",
  dataset_ids = dataset_design %>% filter(topology_id %in% c("linear", "bifurcation")) %>% pull(dataset_id),
  method_id = "move_cells_subedges",
  parameters = list(move_cells_subedges = tibble(n_edges = seq(0, 6), subedge_length_magnification = 1) %>% mutate(id = as.character(n_edges))),
  varied_parameter_id = "n_edges",
  varied_parameter_name = "Number of edges"
)

# not used for now
# move_cells_subedges_magnified <- rule_monotonic(
#   id = "move_cells_subedges_magnified",
#   description = "The longer an extra edges, the lower the score",
#   dataset_ids = dataset_design %>% filter(topology_id %in% c("linear", "bifurcation")) %>% pull(dataset_id),
#   method_id = "move_cells_subedges",
#   parameters = list(move_cells_subedges = tibble(subedge_length_magnification = seq(0,1, 0.2), n_edges = 1) %>% mutate(id = paste0("magnified_", as.character(subedge_length_magnification)))),
#   varied_parameter_id = "subedge_length_magnification"
# )

add_leaf_edges <- rule_monotonic(
  id = "add_leaf_edges",
  description = "Adding extra edges only connected to one existing milestone should lower the score",
  dataset_ids = dataset_design %>% filter(topology_id %in% c("linear", "bifurcation")) %>% pull(dataset_id),
  parameters = list(add_leaf_edges = tibble(n_edges = seq(0, 6)) %>% mutate(id = as.character(n_edges))),
  varied_parameter_id = "n_edges",
  method_id = "add_leaf_edges",
  varied_parameter_name = "Number of edges"
)

add_connecting_edges <- rule_monotonic(
  id = "add_connecting_edges",
  description = "Adding new edges between existing milestones should lower the score",
  dataset_ids = dataset_design %>% filter(topology_id %in% c("linear", "bifurcation")) %>% pull(dataset_id),
  parameters = list(add_connecting_edges = tibble(n_edges = seq(0, 4)) %>% mutate(id = as.character(n_edges))),
  varied_parameter_id = "n_edges",
  method_id = "add_connecting_edges",
  varied_parameter_name = "Number of edges"
)

time_warping_start <- rule_monotonic(
  id = "time_warping_start",
  description = "Warping the positions of the cells within each edge should lower the score",
  dataset_ids = dataset_design %>% filter(topology_id %in% c("linear", "bifurcation")) %>% pull(dataset_id),
  parameters = list(time_warping_start = tibble(warp_magnitude = seq(0, 6)) %>% mutate(id = as.character(warp_magnitude))),
  varied_parameter_id = "warp_magnitude",
  method_id = "time_warping_start",
  varied_parameter_name = "Warp magnitude"
)

time_warping_parabole <- rule_monotonic(
  id = "time_warping_parabole",
  description = "Warping the positions of the cells within each edge should lower the score",
  dataset_ids = dataset_design %>% filter(topology_id %in% c("linear", "bifurcation")) %>% pull(dataset_id),
  parameters = list(time_warping_parabole = tibble(warp_magnitude = seq(1, 20, 2)) %>% mutate(id = as.character(warp_magnitude))),
  varied_parameter_id = "warp_magnitude",
  method_id = "time_warping_parabole",
  varied_parameter_name = "Warp magnitude"
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
        from_topology = factor(topology_id, levels = unique(dataset_design$topology_id)),
        to_topology = factor(param_id, levels = levels(from_topology))
      )

    # calculate scores when topologies stay the same
    top_scores <- scores %>%
      filter(param_id == topology_id) %>%
      select(dataset_id, metric_id, top_score = score)

    # check that when the topology has changed, the score decreased
    scores_different <- scores %>%
      filter(param_id != topology_id) %>%
      left_join(top_scores, c("dataset_id", "metric_id")) %>%
      mutate(conforms = score < top_score)

    conformity <- scores_different %>%
      group_by(metric_id) %>%
      summarise(conforms = all(conforms))

    # get for every kind of topology the trajectory type, used for coloring
    topology_colors <- models %>%
      group_by(param_id) %>%
      slice(1) %>%
      mutate(
        trajectory_type = map(model, "milestone_network") %>% map(dynwrap::classify_milestone_network) %>% map_chr("network_type")
      ) %>%
      left_join(trajectory_types, c(trajectory_type = "id")) %>%
      select(param_id, colour) %>%
      deframe()

    # plot the score differences in pie charts
    plot_scores <- scores %>%
      group_by(from_topology, to_topology, metric_id) %>%
      summarise(score = mean(score)) %>%
      ggplot(aes(to_topology, score)) +
      geom_hline(aes(yintercept = y), data_frame(y = seq(0, 1, 0.2)), colour = "gray") +
      geom_bar(aes(fill = to_topology), width = 1, stat = "identity") +
      scale_x_discrete(NULL, labels = NULL) +
      scale_y_continuous(position = "right") +
      scale_color_manual(values = topology_colors) +
      scale_fill_manual(values = topology_colors) +
      facet_grid(from_topology ~ metric_id, switch = "y", labeller = function(tib) {
        if ("metric_id" %in% colnames(tib)) {
          tib$metric_id <- map(tib$metric_id, label_metric, parse = TRUE)
        }
        if (is.factor(tib[[1]])) tib[[1]] <- as.character(tib[[1]])
        tib
      }) +
      coord_polar() +
      theme_bw() +
      theme(
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_rect(colour = "grey"),
        strip.text.y = element_text(angle = 180)
      )

    # plot every topology
    models <- models %>%
      group_by(param_id) %>%
      filter(row_number() == 1) %>%
      ungroup() %>%
      mutate(topology = factor(param_id, levels = levels(scores$to_topology))) %>%
      arrange(topology)

    plot_datasets <- map2(models$topology, models$model, function(title, model) {
      dynplot::plot_topology(model) + ggtitle(title)
    }) %>% patchwork::wrap_plots(ncol = 4)

    lst(
      conformity,
      plot_scores,
      plot_datasets
    )
  }
)


# we set a seed here so that the shuffling between the milestone and edge
milestone_vs_edge <- lst(
  id = "milestone_vs_edge",
  description = "Changing the cell positions should influence the score similarly when the cells are in milestones (as is the case in real datasets) or on the edges between milestones (as is the case in synthetic datasets)",
  parameters = lst(
    switch_cells = switch_cells$parameters$switch_cells %>% mutate(seed = 1, id = paste0("fixed_seed_", row_number())),
    switch_cells_grouped = switch_cells
  ),
  crossing = crossing(
    dataset_id = unique(switch_cells$crossing$dataset_id),
    method_id = names(parameters),
    param_id = parameters$switch_cells$id
  ),
  assessment = function(scores, rule, models) {
    # check if scores are highly correlated
    conformity <- scores %>%
      spread(method_id, score) %>%
      group_by(metric_id) %>%
      summarise(cor = cor(switch_cells, switch_cells_grouped)) %>%
      mutate(conforms = ifelse(is.na(cor), TRUE, cor > 0.8))

    # plot the relation between the scores on the continuous and grouped datasets
    plots <- scores %>%
      nest(-metric_id, .key = "scores") %>%
      mutate(metric_id = as.character(metric_id)) %>%
      pmap(function(metric_id, scores) {
        limits <- sort(limits_metric(metric_id))
        breaks <- unname(limits)

        cor <- conformity$cor[conformity$metric_id == metric_id]

        scores %>%
          spread(method_id, score) %>%
          ggplot(aes(switch_cells, switch_cells_grouped)) +
            geom_point(alpha = 0.5) +
            geom_smooth(se = FALSE, method = lm) +
          theme_bw() +
          geom_hline(aes(yintercept = value), data = limits_metric(metric_id) %>% enframe(), linetype = "dashed") +
          geom_vline(aes(xintercept = value), data = limits_metric(metric_id) %>% enframe(), linetype = "dashed") +
          annotate(
            "label",
            label = glue::glue("Correlation: {round(cor, 2)}"),
            x = first(limits) + diff(limits)*0.1,
            y = first(limits) + diff(limits)*0.9,
            hjust = 0,
            vjust = 0
          ) +
          scale_y_continuous("Score with cells on milestones", limits = limits, breaks = breaks) +
          scale_x_continuous("Score with cells on edges", limits = limits, breaks = breaks) +
          theme(
            panel.grid.minor = element_blank(),
            panel.border = element_blank()
          ) +
          labs(
            title = label_metric(metric_id, parse = TRUE),
            parse = TRUE
          )

      })
    plot_scores <- patchwork::wrap_plots(plots, ncol = 4)

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




rule_combined <- function(
  id,
  description,
  parameters,
  dataset_ids,
  method_ids
) {
  testthat::expect_true(length(method_ids) == 3)

  middle_method_id1 <- rlang::sym(method_ids[1])
  middle_method_id2 <- rlang::sym(method_ids[2])
  end_method_id <- rlang::sym(method_ids[3])

  assessment <- function(scores, rule, models) {
    scores <- scores %>%
      mutate(
        method_id = factor(method_id, levels = c("identity", method_ids))
      )

    # check if scores are highly correlated
    conformity <- scores %>%
      select(-param_id) %>%
      spread(method_id, score) %>%
      group_by(metric_id) %>%
      summarise(
        end_quantile = quantile(!!end_method_id, 0.75),
        mid_quantile1 = quantile(!!middle_method_id1, 0.25),
        mid_quantile2 = quantile(!!middle_method_id2, 0.25),
        conforms = (end_quantile < mid_quantile1) & (end_quantile < mid_quantile2)
      )

    # plot the scores of the two individual methods and the combined
    colors <- set_names(c("#DDDDDD", "#FF4136", "#0074D9", "#2ECC40"), c("identity", method_ids))

    plots <- scores %>%
      nest(-metric_id, .key = "scores") %>%
      mutate(metric_id = as.character(metric_id)) %>%
      pmap(function(metric_id, scores) {
        limits <- sort(limits_metric(metric_id))
        breaks <- c(unname(limits), scores %>% group_by(method_id) %>% summarise(score = median(score)) %>% pull(score)) %>% round(2)

        scores %>%
          ggplot(aes(method_id, score, fill = method_id)) +
          geom_boxplot() +
          scale_fill_manual(values = colors) +
          geom_hline(aes(yintercept = value), data = limits_metric(metric_id) %>% enframe(), linetype = "dashed") +
          scale_y_continuous(NULL, limits = limits, breaks = breaks) +
          scale_x_discrete(NULL, labels = c("identity", "a", "b", "a+b")) +
          labs(
            title = label_metric(metric_id, parse = TRUE),
            parse = TRUE
          ) +
          theme_bw() +
          theme(
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            legend.position = "none",
            axis.text.x = element_text(angle = 30, hjust = 1)
          )
      })
    plot_scores <- patchwork::wrap_plots(plots, nrow = 1)

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

  lst(
    id = id,
    description = description,
    parameters = parameters,
    crossing =  map2_df(
      names(parameters),
      parameters,
      function(method_id, parameters) {
        crossing(
          method_id,
          param_id = parameters$id,
          dataset_id = dataset_ids
        )
      }
    ),
    assessment = assessment
  )
}

combined_position_topology <- rule_combined(
  id = "combined_position_topology",
  description = "Changing both the topology and the cell positions should lower the score more than any of the two individually",
  parameters = list(
    identity = tibble(id = "default"),
    switch_cells = tibble(switch_perc = 0.25, id = "combination"),
    add_connecting_edges = tibble(n_edges = 1, id = "combination"),
    switch_cells_and_add_connecting_edges = tibble(switch_perc = 0.25, n_edges = 1, id = "combination")
  ),
  method_ids = c("switch_cells", "add_connecting_edges", "switch_cells_and_add_connecting_edges"),
  dataset_ids = dataset_design %>% filter(num_cells == 100) %>% filter(topology_id %in% names(topologies)) %>% pull(dataset_id)
)

combined_merge_bifurcation_switch_cells <- rule_combined(
  id = "combined_merge_bifurcation_switch_cells",
  description = "Merging the two branches of a bifurcation and the cells positions should lower the score more than any of the two individually",
  parameters = list(
    identity = tibble(id = "default"),
    switch_cells = tibble(switch_perc = 0.25, id = "combination"),
    merge_bifurcation = tibble(id = "default"),
    switch_cells_and_merge_bifurcation = tibble(switch_perc = 0.25, id = "combination")
  ),
  method_ids = c("switch_cells", "merge_bifurcation", "switch_cells_and_merge_bifurcation"),
  dataset_ids = dataset_design %>% filter(num_cells == 100, topology_id == "bifurcation_simple") %>% pull(dataset_id)
)


# combine rules
rules <- list(
  equal_identity,
  switch_cells_edgewise,
  merge_bifurcation,
  concatenate_bifurcation,
  break_cycle,
  join_linear,
  split_linear,
  switch_cells,
  filter_cells,
  move_cells_subedges,
  add_leaf_edges,
  add_connecting_edges,
  switch_edges,
  time_warping_start,
  time_warping_parabole,
  shuffle_lengths,
  change_topology,
  milestone_vs_edge,
  combined_position_topology,
  combined_merge_bifurcation_switch_cells
) %>% map(~list(.) %>% list_as_tibble()) %>% bind_rows()

# rules <- rules %>% filter(id %in% c("equal_identity"))

# check rules for contents
walkdf(rules, function(rule) {
  testthat::expect_true(all(
    c("id", "description", "crossing", "assessment") %in% names(rule)
  ))
})
