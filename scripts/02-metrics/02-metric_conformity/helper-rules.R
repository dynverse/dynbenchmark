#' Helper functions containing the rules



approx_unique <- function(x, tolerance = .Machine$double.eps ^ 0.5) {
  abs(first(sort(x)) - last(sort(x))) < tolerance
}

approx_equal <- function(x, y, tolerance = .Machine$double.eps ^ 0.5) {
  abs(x - y) < tolerance
}

# is the value of y monotonically approximately increasing/decreasing with increasing x?
is_monotonic <- function(x, y, decreasing = TRUE) {
  if (decreasing) {
    all(diff(y[order(x)]) <= 0.01) && (first(y) > last(y)) # add a little bit of room for noise here
  } else {
    all(diff(y[order(x)]) >= -0.01) && (first(y) < last(y))
  }
}

# correlation which is 1 if there is only one unique value
cor0 <- function(x, y) {
  if (length(unique(x)) == 1 || length(unique(y)) == 1) {
    1
  } else {
    cor(x, y)
  }
}

# is the value of y monotonic with x AND changing from the smallest x to the largest x?
is_monotonic_changing <- function(x, y, decreasing = TRUE) {
  is_monotonic(x, y, decreasing) && (
    if (decreasing) {
      all(y[which.min(x)] > y[which.max(x)])
    } else {
      all(y[which.min(x)] < y[which.max(x)])
    }
  )
}

equal_identity <- lst(
  id = "equal_identity",
  name = "Same score on identity",
  description = "The score should be approximately the same when comparing the trajectory to itself",
  observation = "Metrics which contain some stochasticity (random forest based metrics in particular), usually do not conform to this rule, even though their scores are still consistently high.",
  conforms_if = "0.99 \\leqslant \\mathit{score} \\leqslant 1",
  crossing = crossing(
    dataset_id = dataset_design %>% filter(topology_id %in% names(dynbenchmark:::topologies_with_same_n_milestones), num_cells > 50) %>% pull(dataset_id),
    method_id = "identity"
  ),
  assessment = function(scores) {
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

    plot_scores <- scores %>%
      ggplot(aes(metric_id, score)) +
      geom_boxplot() +
      geom_hline(aes(yintercept = cutoff), data = metric_cutoffs %>% enframe("metric_id", "cutoff"), linetype = "dashed") +
      scale_y_continuous(limits = c(0.9, 1)) +
      scale_x_discrete(NULL, labels = labels) +
      theme_bw() +
      theme(
        panel.grid.minor = element_blank(),
        panel.border = element_blank()
      )

    plot_scores$width <- length(unique(scores$metric_id)) * 1.2
    plot_scores$height <- 4
    plot_scores$caption <- glue::glue("Score values of the different metrics across {length(unique(scores$dataset_id))} datasets.")

    lst(
      conformity = scores %>%
        mutate(higher_or_equal = score >= metric_cutoffs[as.character(metric_id)]) %>%
        group_by(metric_id) %>%
        summarise(conforms = all(higher_or_equal))
      ,
      plot_scores
    )
  },
  plot_datasets = function(models) {
    identity <- dataset_design %>%
      filter(dataset_id %in% crossing$dataset_id, num_cells == 100) %>%
      pull(dataset) %>%
      {.[[1]]()}

    plot_datasets <- plot_graph(identity) + ggtitle("Reference") + theme(legend.position = "none", plot.title = element_text(hjust = 0.5))

    plot_datasets$width <- 4
    plot_datasets$height <- 4

    plot_datasets$caption <- "Example trajectory that was used to assess this rule."

    plot_datasets
  }
)


# create a rule which checks whether the score decreases compared to gold standard
rule_lower <- function(
  id,
  name,
  description,
  dataset_ids,
  method_id,
  ...
) {
  lst(
    id = id,
    name = name,
    description = description,
    ...,
    conforms_if = "\\mathit{score}_{\\textit{identity}} > \\mathit{score}_{\\textit{prediction}}",
    crossing = crossing(
      dataset_id = dataset_ids,
      method_id = c("identity", method_id)
    ),
    assessment = function(scores) {
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
        summarise(conforms = all(difference <= 0) && mean(difference < 0))

      # plot the scores using a dot plot
      differences_mean <- differences %>%
        group_by(metric_id) %>%
        summarise(difference = mean(difference))

      labels <- set_names(levels(differences$metric_id)) %>%
        map(label_metric, parse = TRUE) %>%
        map_chr(as.character) %>%
        parse(text = .)

      plot_scores <- differences %>%
        ggplot(aes(metric_id, difference)) +
        ggbeeswarm::geom_quasirandom() +
        geom_hline(yintercept = 0, linetype = "dashed") +
        geom_point(data = differences_mean, color = "red", size = 20, shape = "-") +
        scale_x_discrete(NULL, position = "top", labels = labels) +
        scale_y_continuous("Score difference", limits = c(-1, max(differences$difference, 0))) +
        theme_bw() +
        theme(
          panel.grid.minor = element_blank(),
          panel.border = element_blank()
        )

      plot_scores$width <- length(unique(scores$metric_id)) * 1.2
      plot_scores$height <- 4
      plot_scores$caption <- glue::glue("Differences in scores of {nrow(differences)} datasets before and after perturbation. Red bar gives the mean.")

      lst(
        conformity,
        plot_scores
      )
    },
    plot_datasets = function() {
      # get identity dataset
      identity <- dataset_design %>%
        filter(dataset_id %in% dataset_ids, num_cells == 100) %>%
        pull(dataset) %>%
        first() %>%
        invoke()

      # get method
      method <- perturbation_methods %>%
        filter(id == method_id) %>%
        pull(fun) %>%
        first() %>%
        invoke()

      # get perturbed
      perturbed <- infer_trajectory(identity, method)

      grouping <- dynwrap::group_onto_trajectory_edges(identity)
      plot_datasets <- map2(
        list(identity, perturbed),
        c("Reference", name),
        function(model, title) {
          plot_graph(model, grouping=grouping) + ggtitle(label_long(title)) + theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
        }) %>%
        patchwork::wrap_plots(nrow = 1)

      plot_datasets$width <- 4 * 2
      plot_datasets$height <- 4
      plot_datasets$caption <- "Example dataset before and after perturbation."

      plot_datasets
    }
  )
}

rule_monotonic <- function(
  id,
  name,
  description,
  dataset_ids,
  method_id,
  parameters,
  varied_parameter_id,
  varied_parameter_name = varied_parameter_id,
  varied_parameter_labeller = function(x) x,
  ...
) {
  assessment <- function(scores) {
    varied_parameter_sym <- rlang::sym(varied_parameter_id)

    # join scores with the parameters
    scores <- scores %>%
      left_join(parameters[[method_id]], by=c("param_id" = "id"))

    scores_mean <- scores %>%
      group_by(!!varied_parameter_sym, metric_id) %>%
      summarise(score = mean(score)) %>%
      ungroup()

    # check whether the score decreases monotonically
    conformity <- scores_mean %>%
      group_by(metric_id) %>%
      summarise(conforms = is_monotonic_changing(!!varied_parameter_sym, score))

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

    plot_scores$width <- 12
    plot_scores$height <- ceiling(length(plots)/4) * 3
    plot_scores$caption <- glue::glue("Score values at different extents of the perturbation across {length(unique(scores$dataset_id))} datasets. Red line denotes the mean values.")

    lst(
      conformity,
      plot_scores
    )
  }

  plot_datasets <- function() {
    varied_parameter_sym <- rlang::sym(varied_parameter_id)

    # get identity dataset
    identity <- dataset_design %>%
      filter(dataset_id %in% dataset_ids, num_cells == 100) %>%
      pull(dataset) %>%
      first() %>%
      invoke()

    # get method
    method <- perturbation_methods %>%
      filter(id == method_id) %>%
      pull(fun) %>%
      first() %>%
      invoke()

    # get the models
    parameters_chosen <- parameters[[method_id]] %>%
      filter(
        !!varied_parameter_sym %in% c(min(!!varied_parameter_sym), sort(!!varied_parameter_sym)[round(n()/2)], max(!!varied_parameter_sym))
      )
    models <- mapdf(parameters_chosen %>% select(-id), infer_trajectory, dataset = identity, method = method)

    # do the actual plotting
    grouping <- group_onto_trajectory_edges(models[[1]])
    plot_datasets <- map2(
      models,
      glue::glue("{varied_parameter_name}: {varied_parameter_labeller(parameters_chosen[[varied_parameter_id]])}"),
      function(model, title) {
        plot_graph(model, grouping=grouping) + ggtitle(label_long(title)) + theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
      }) %>%
      patchwork::wrap_plots()

    plot_datasets$width <- 12
    plot_datasets$height <- 4
    plot_datasets$caption <- "Lowly (left), moderatly (middle) and highly (right) perturbed example dataset."

    plot_datasets
  }

  lst(
    id,
    description,
    name,
    conforms_if = paste0("\\mathit{monotonic} \\left( \\textit{", varied_parameter_name, "}, \\overline{\\mathit{score}}_{\\textit{", varied_parameter_name, "}} \\right)"),
    parameters,
    crossing = crossing(
      dataset_id = dataset_ids,
      method_id = method_id,
      param_id = parameters[[method_id]]$id
    ),
    assessment,
    plot_datasets,
    ...
  )
}

rule_combined <- function(
  id,
  name,
  description,
  parameters,
  dataset_ids,
  method_ids,
  ...
) {
  testthat::expect_true(length(method_ids) == 3)

  a <- rlang::sym(method_ids[1])
  b <- rlang::sym(method_ids[2])
  a_plus_b <- rlang::sym(method_ids[3])

  assessment <- function(scores) {
    scores <- scores %>%
      mutate(
        method_id = factor(method_id, levels = c("identity", method_ids))
      )

    # check if scores are lower in a, b and more so in a+b
    conformity <- scores %>%
      select(-param_id) %>%
      spread(method_id, score) %>%
      mutate(
        lower_a = !!a < identity,
        lower_b = !!b < identity,
        lower_a_plus_b = (!!a_plus_b < !!a) && (!!a_plus_b < !!b)
      ) %>%
      group_by(metric_id) %>%
      summarise(
        conforms = all(lower_a) && all(lower_b) && all(lower_a_plus_b)
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
    plot_scores <- patchwork::wrap_plots(plots, nrow = 2)

    plot_scores$width <- length(unique(scores$metric_id)) * 1.5 / 2
    plot_scores$height <- 8
    plot_scores$caption <- glue::glue("Score values before perturbation (identity), with any of the two perturbations (a and b) and both perturbations combined (a+b). The upper whisker of the boxplot extends from the hinge (75% percentile) to the largest value, no further than 1.5× the IQR from the hinge. The lower whisker extends from the hinge (25% percentile) to the smallest value, at most 1.5× the IQR of the hinge. We used {sum(scores$metric_id == first(scores$metric_id))} different datasets.")

    lst(
      conformity,
      plot_scores
    )
  }

  plot_datasets <- function() {
    # get identity dataset
    identity <- dataset_design %>%
      filter(dataset_id %in% dataset_ids, num_cells == 100) %>%
      pull(dataset) %>%
      first() %>%
      invoke()

    # get method
    methods <- perturbation_methods %>%
      slice(match(method_ids, id)) %>%
      pull(fun) %>%
      map(invoke)

    # get the models
    parameters_chosen <- parameters[method_ids] %>% map(~select(., -id)) %>% map(extract_row_to_list, 1)
    models <- map2(methods, parameters_chosen, infer_trajectory, dataset = identity)

    # plot the four models
    grouping <- group_onto_trajectory_edges(identity)

    plot_datasets <- map2(
      c(list(identity), models),
      paste0(c("identity", method_ids), c("", "(a) ", "(b) ", "(a+b) ")),
      function(model, title) {
        plot_graph(model, grouping = grouping) + ggtitle(label_long(title)) + theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
      }) %>%
      patchwork::wrap_plots(ncol = 4)

    plot_datasets$width <- 3 * 4
    plot_datasets$height <- 3
    plot_datasets$caption <- "Example dataset before perturbation (identity), with any of the two perturbations (a and b) and both perturbations combined (a+b)."

    plot_datasets
  }

  lst(
    id = id,
    name = name,
    description = description,
    conforms_if = "\\mathit{score}_{identity} > \\mathit{score}_a \\land \\mathit{score}_{identity} > \\mathit{score}_b \\land \\mathit{score}_{a} > \\mathit{score}_{a+b} \\land \\mathit{score}_{b} > \\mathit{score}_{a+b}",
    conforms_if = "The scores of (a) and (b) are all lower than identity, and the scores of (a+b) are all lower than (a) and (b) on the same datasets",
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
    assessment,
    plot_datasets,
    ...
  )
}









shuffle_cells_edgewise <- rule_lower(
  id = "shuffle_cells_edgewise",
  name = "Local cell shuffling",
  description = "Shuffling the positions of cells within each edge should lower the score. This is equivalent to changing the cellular position locally.",
  dataset_ids = dataset_design %>% filter(topology_id %in% names(dynbenchmark:::topologies_with_same_n_milestones), cell_positioning != "milestones", num_cells > 50) %>% pull(dataset_id),
  method_id = "shuffle_cells_edgewise",
  observation = "Metrics which do not look at the cellular positioning, or group the cells within branches or milestones, do not conform to this rule."
)

shuffle_edges <- rule_monotonic(
  id = "shuffle_edges",
  name = "Edge shuffling",
  description = "Shuffling the edges in the milestone network should lower the score. This is equivalent to changing the cellular positions only globally.",
  dataset_ids = dataset_design %>% filter(topology_id %in% names(dynbenchmark:::topologies_with_same_n_milestones)) %>% pull(dataset_id),
  method_id = "shuffle_edges",
  parameters = list(shuffle_edges = tibble(shuffle_perc = seq(0, 1, 0.5)) %>% mutate(id = as.character(shuffle_perc*10))),
  varied_parameter_id = "shuffle_perc",
  varied_parameter_name = "shuffled edges",
  varied_parameter_labeller = scales::percent,
  observation = "Metrics which only look at the topology do not conform to this rule."
)

shuffle_cells <- rule_monotonic(
  id = "shuffle_cells",
  name = "Local and global cell shuffling",
  description = "Shuffling the positions of cells should lower the score. This is equivalent to changing the cellular position both locally and globally.",
  dataset_ids = dataset_design %>% filter(topology_id %in% names(dynbenchmark:::topologies_with_same_n_milestones)) %>% pull(dataset_id),
  method_id = "shuffle_cells",
  parameters = list(shuffle_cells = tibble(shuffle_perc = seq(0, 1, 0.1)) %>% mutate(id = as.character(shuffle_perc*10))),
  varied_parameter_id = "shuffle_perc",
  varied_parameter_name = "shuffled cells",
  varied_parameter_labeller = scales::percent,
  observation = "Most metrics that look at the position of each cell conform to this rule."
)

combined_local_global_position_change <- rule_combined(
  id = "combined_local_global_position_change",
  name = "Changing positions locally and/or globally",
  description = "Changing the cellular position locally AND globally should lower the score more than any of the two individually.",
  parameters = list(
    identity = tibble(id = "default"),
    shuffle_edges = shuffle_edges$parameters$shuffle_edges %>% filter(shuffle_perc == 1),
    shuffle_cells_edgewise = tibble(id = "default"),
    shuffle_cells = shuffle_cells$parameters$shuffle_cells %>% filter(shuffle_perc == 1)
  ),
  method_ids = c("shuffle_edges", "shuffle_cells_edgewise", "shuffle_cells"),
  dataset_ids = dataset_design %>% filter(topology_id %in% names(dynbenchmark:::topologies_with_same_n_milestones), cell_positioning != "milestones", num_cells > 50) %>% pull(dataset_id),
  observation = "Because the topology remains the same, the topology scores do not conform to this rule. Also the clustering based scores have some difficulties with this rule."
)

filter_cells <- rule_monotonic(
  id = "filter_cells",
  name = "Cell filtering",
  description = "Removing cells from the trajectory should lower the score",
  dataset_ids = dataset_design %>% filter(topology_id %in% names(dynbenchmark:::topologies_with_same_n_milestones)) %>% pull(dataset_id),
  method_id = "filter_cells",
  parameters = list(filter_cells = tibble(filter_perc = seq(0, 1, 0.1)) %>% mutate(id = as.character(filter_perc*10))),
  varied_parameter_id = "filter_perc",
  varied_parameter_name = "Filtered cells",
  varied_parameter_labeller = scales::percent,
  observation = "Metrics which look at the topology do not conform to this rule."
)

remove_divergence_regions <- rule_lower(
  id = "remove_divergence_regions",
  name = "Removing divergence regions",
  description = "Removing divergence regions should lower the score",
  dataset_ids = dataset_design %>% filter(
    topology_id == "bifurcation_tented",
    cell_positioning != "milestones",
    num_cells >= 100
  ) %>% pull(dataset_id),
  method_id = "remove_divergence_regions",
  observation = glue::glue("Both {label_metric('F1_branches', 'latex')} and {label_metric('edge_flip', 'latex')} fail here because neither the topology nor the branch assignment changes. Moreover, the decreases in score are relatively minor for all metrics, given that the impact of the positions of the cells is only minimal.")
)

time_warping_start <- rule_monotonic(
  id = "time_warping_start",
  name = "Move cells to start milestone",
  description = "Moving the cells closer to their start milestone should lower the score. Cells were moved closer to the start milestone using $\\textit{percentage}_{\\textit{new}} = \\textit{percentage}^{\\textit{warp magnitude}}$",
  dataset_ids = dataset_design %>% filter(topology_id %in% names(dynbenchmark:::topologies_with_same_n_milestones)) %>% pull(dataset_id),
  parameters = list(time_warping_start = tibble(warp_magnitude = seq(0, 6)) %>% mutate(id = as.character(warp_magnitude))),
  varied_parameter_id = "warp_magnitude",
  method_id = "time_warping_start",
  varied_parameter_name = "Warp magnitude",
  observation = glue::glue("Both {label_metric('F1_branches', 'latex')} and topology scores fail here because neither the topology nor the branch assignment changes. The score decreases only slightly for all the other metrics, given that only the relative distances change between cells, but not their actual ordering.")
)

time_warping_parabole <- rule_monotonic(
  id = "time_warping_parabole",
  name = "Move cells to closest milestone",
  description = "Moving the cells closer to their nearest milestone should lower the score",
  dataset_ids = dataset_design %>% filter(topology_id %in% names(dynbenchmark:::topologies_with_same_n_milestones)) %>% pull(dataset_id),
  parameters = list(time_warping_parabole = tibble(warp_magnitude = seq(1, 20, 2)) %>% mutate(id = as.character(warp_magnitude))),
  varied_parameter_id = "warp_magnitude",
  method_id = "time_warping_parabole",
  varied_parameter_name = "Warp magnitude",
  observation = glue::glue("Both {label_metric('F1_branches', 'latex')} and topology scores fail here because neither the topology nor the branch assignment changes. The score decreases only slightly for all the other metrics, given that only the relative distances change between cells, but not their actual ordering.")
)

shuffle_lengths <- rule_lower(
  id = "shuffle_lengths",
  name = "Length shuffling",
  description = "Shuffling the lengths of the edges of the milestone network should lower the score.",
  dataset_ids = dataset_design %>% filter(
    topology_id %in% names(dynbenchmark:::topologies_with_same_n_milestones),
    cell_positioning != "milestones",
    num_cells >= 100
  ) %>% pull(dataset_id),
  method_id = "shuffle_lengths",
  observation = "Only the correlation between geodesic distances is consistently decreases when the lengths of the edges change."
)

move_cells_subedges <- rule_monotonic(
  id = "move_cells_subedges",
  name = "Cells into small subedges",
  description = "Moving some cells into short subedges should lower the score",
  dataset_ids = dataset_design %>% filter(topology_id %in% names(dynbenchmark:::topologies_with_same_n_milestones)) %>% pull(dataset_id),
  method_id = "move_cells_subedges",
  parameters = list(move_cells_subedges = tibble(n_edges = seq(0, 6), subedge_length_magnification = 1) %>% mutate(id = as.character(n_edges))),
  varied_parameter_id = "n_edges",
  varied_parameter_name = "Number of added edges",
  observation = "This rule is primarily captured by the scores looking at the topology and clustering quality."
)

add_leaf_edges <- rule_monotonic(
  id = "add_leaf_edges",
  name = "New leaf edges",
  description = "Adding new edges only connected to one existing milestone should lower the score",
  dataset_ids = dataset_design %>% filter(topology_id %in% names(dynbenchmark:::topologies_with_same_n_milestones)) %>% pull(dataset_id),
  parameters = list(add_leaf_edges = tibble(n_edges = seq(0, 6)) %>% mutate(id = as.character(n_edges))),
  varied_parameter_id = "n_edges",
  method_id = "add_leaf_edges",
  varied_parameter_name = "Number of edges",
  observation = "As the positions of the cells are not affected, only metrics which investigate the clustering quality and topology conform to this rule."
)

add_connecting_edges <- rule_monotonic(
  id = "add_connecting_edges",
  name = "New connecting edges",
  description = "Adding new edges between existing milestones should lower the score",
  dataset_ids = dataset_design %>% filter(topology_id %in% names(dynbenchmark:::topologies_with_same_n_milestones)) %>% pull(dataset_id),
  parameters = list(add_connecting_edges = tibble(n_edges = seq(0, 4)) %>% mutate(id = as.character(n_edges))),
  varied_parameter_id = "n_edges",
  method_id = "add_connecting_edges",
  varied_parameter_name = "Number of edges",
  observation = glue::glue("Even though the positions of the cells do not change, the {label_metric('correlation', 'latex')} still conforms to this rule because new edges can create shortcuts which will affect the geodesic distances between cells. Apart from this, metrics which investigate the clustering quality and topology also conform to this rule.")
)

combined_position_topology <- rule_combined(
  id = "combined_position_topology",
  name = "Changing topology and cell position",
  description = "Changing both the topology and the cell positions should lower the score more than any of the two individually",
  parameters = list(
    identity = tibble(id = "default"),
    shuffle_cells = tibble(shuffle_perc = 0.25, id = "combination"),
    add_connecting_edges = tibble(n_edges = 1, id = "combination"),
    shuffle_cells_and_add_connecting_edges = tibble(shuffle_perc = 0.25, n_edges = 1, id = "combination")
  ),
  method_ids = c("shuffle_cells", "add_connecting_edges", "shuffle_cells_and_add_connecting_edges"),
  dataset_ids = dataset_design %>% filter(num_cells == 100) %>% filter(topology_id %in% names(dynbenchmark:::topologies_with_same_n_milestones)) %>% pull(dataset_id),
  observation = glue::glue("Most metrics have problems with this rule as they focus on either the cellular positions or the topology individually. Only the {label_metric('correlation')} and {label_metric('geom_mean', 'latex')} consistently conform to this rule.")
)

merge_bifurcation <- rule_lower(
  id = "merge_bifurcation",
  name = "Bifurcation merging",
  description = "Merging the two branches after a bifurcation point should lower the score",
  dataset_ids = dataset_design %>% filter(topology_id == "bifurcation_simple") %>% pull(dataset_id),
  method_id = "merge_bifurcation",
  observation = "This changes both the cellular ordering and the topology so most metrics are affected."
)

combined_merge_bifurcation_shuffle_cells <- rule_combined(
  id = "combined_merge_bifurcation_shuffle_cells",
  name = "Bifurcation merging and changing cell positions",
  description = "Merging the two branches of a bifurcation and changing the cells positions should lower the score more than any of the two individually",
  parameters = list(
    identity = tibble(id = "default"),
    shuffle_cells = tibble(shuffle_perc = 0.25, id = "combination"),
    merge_bifurcation = tibble(id = "default"),
    shuffle_cells_and_merge_bifurcation = tibble(shuffle_perc = 0.25, id = "combination")
  ),
  method_ids = c("shuffle_cells", "merge_bifurcation", "shuffle_cells_and_merge_bifurcation"),
  dataset_ids = dataset_design %>% filter(num_cells == 100, topology_id == "bifurcation_simple") %>% pull(dataset_id),
  observation = "Only metrics which look at the topology do not conform to this rule."
)

concatenate_bifurcation <- rule_lower(
  id = "concatenate_bifurcation",
  name = "Bifurcation concatentation",
  description = "Concatenating one branch of a bifurcation to the other bifurcation branch should lower the score",
  dataset_ids = dataset_design %>% filter(topology_id == "bifurcation_simple", num_cells > 50) %>% pull(dataset_id),
  method_id = "concatenate_bifurcation",
  observation = "This changes both the cellular ordering and the topology so most metrics conform to this rule."
)

break_cycle <- rule_lower(
  id = "break_cycle",
  name = "Cycle breaking",
  description = "Breaking a cyclic trajectory should lower the score",
  dataset_ids = dataset_design %>% filter(topology_id == "cycle") %>% pull(dataset_id),
  method_id = "break_cycle",
  observation = glue::glue("Because the actual positions of the cells nor the branch assignment change, both the MSE metrics and the {label_metric('F1_branches', 'latex')} do not conform to this rule.")
)

join_linear <- rule_lower(
  id = "join_linear",
  name = "Linear joining",
  description = "Joining the two ends of a linear trajectory should lower the score",
  dataset_ids = dataset_design %>% filter(topology_id == "linear") %>% pull(dataset_id),
  method_id = "join_linear",
  observation = glue::glue("Because the positions of the cells can be perfectly predicted, the MSE metrics do not conform to this rule. Furthermore, because the branch assignment change stays the same, the {label_metric('F1_branches', 'latex')} also does not conform to this rule.")
)

split_linear <- rule_lower(
  id = "split_linear",
  name = "Linear splitting",
  description = "Splitting a linear trajectory into a bifurcation should lower the score",
  dataset_ids = dataset_design %>% filter(topology_id == "linear", num_cells > 50) %>% pull(dataset_id),
  method_id = "move_terminal_branch",
  observation = "Only the MSE metrics do not conform to this rule as the positions of the cells can be perfectly predicted in the gold standard given the prediction."
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


change_topology <- lst(
  id = "change_topology",
  name = "Change of topology",
  description = "Changing the topology of the trajectory should lower the score",
  conforms_if = "\\mathit{score}_{\\textit{same topology}} > \\mathit{score}_{\\textit{different topology}}",
  parameters = list(change_topology = tibble(topology_id = names(dynbenchmark:::topologies_with_same_n_milestones), id = topology_id)),
  crossing = crossing(
    dataset_id = dataset_design %>% filter(num_cells == 100) %>% filter(topology_id %in% names(dynbenchmark:::topologies_with_same_n_milestones)) %>% pull(dataset_id),
    method_id = "change_topology",
    param_id = parameters[[method_id]]$id
  ),
  assessment = function(scores) {
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
    topology_trajectory_types <- dataset_design %>%
      group_by(topology_id) %>%
      slice(1) %>%
      select(topology_id, topology_model) %>%
      deframe() %>%
      map(mutate, directed = TRUE, length = 1) %>%
      map(dynwrap::classify_milestone_network) %>%
      map_chr("network_type")

    topology_colors <- trajectory_types %>%
      select(id, colour) %>%
      deframe() %>%
      .[topology_trajectory_types] %>%
      set_names(names(topology_trajectory_types))

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
      scale_fill_manual("Predicted topology", values = topology_colors) +
      facet_grid(from_topology ~ metric_id, switch = "y", labeller = function(tib) {
        if ("metric_id" %in% colnames(tib)) {
          tib$metric_id <- map(tib$metric_id, label_metric, parse = TRUE, format = "plotmath")
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

    plot_scores$width <- length(unique(scores$metric_id))*1.5
    plot_scores$height <- length(unique(scores$from_topology))*1.5
    plot_scores$caption <- "Score values on different topologies (left)."

    lst(
      conformity,
      plot_scores
    )
  },
  plot_datasets = function() {
    models <- dataset_design %>%
      filter(dataset_id %in% crossing$dataset_id) %>%
      group_by(topology_id) %>%
      slice(1) %>%
      select(topology_id, dataset) %>%
      deframe() %>%
      map(invoke)

    plot_datasets <- map2(names(models), models, function(title, model) {
      dynplot::plot_topology(model) +
        ggtitle(title) +
        theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
    }) %>% patchwork::wrap_plots(ncol = 4)

    plot_datasets$width <- 3 * 4
    plot_datasets$height <- 3 * ceiling(length(models) / 4)
    plot_datasets$caption <- "The different trajectory topologies that were used to compare the metrics."

    plot_datasets
  },
  observation = glue::glue("Because the positions of the cells can be perfectly predicted, the MSE metrics do not conform to this rule. Furthermore, because the branch assignment change stays the same, the {label_metric('F1_branches', 'latex')} also does not conform to this rule.")
)




cell_gathering <- lst(
  id = "cell_gathering",
  name = "Cells on milestones vs edges",
  description = "A score should behave similarly both when cells are located on the milestones (as is the case in real datasets) or on the edges between milestones (as is the case in synthetic datasets).",
  conforms_if = "\\mathit{corr} \\left( \\mathit{score}_{\\textit{edges}} , \\mathit{score}_{\\textit{milestones}} \\right) > 0.8",
  parameters = shuffle_cells$parameters,
  crossing = shuffle_cells$crossing,
  assessment = function(scores) {
    scores_spread <- scores %>%
      left_join(dataset_design, "dataset_id") %>%
      select(-dataset_id) %>%
      spread(cell_positioning, score)

    if (any(xor(is.na(scores_spread$edges), is.na(scores_spread$milestones)))) stop("Spreading the edge and milestone scores failed for some datasets")

    # check if scores are highly correlated
    conformity <- scores_spread %>%
      group_by(metric_id) %>%
      summarise(cor = cor0(milestones, edges)) %>%
      mutate(conforms = ifelse(is.na(cor), TRUE, cor > 0.8))

    # plot the relation between the scores on the continuous and grouped datasets
    plots <- scores_spread %>%
      nest(-metric_id, .key = "scores") %>%
      mutate(metric_id = as.character(metric_id)) %>%
      pmap(function(metric_id, scores_spread) {
        limits <- sort(limits_metric(metric_id))
        breaks <- unname(limits)

        cor <- conformity$cor[conformity$metric_id == metric_id]

        scores_spread %>%
          ggplot(aes(edges, milestones)) +
            geom_point(alpha = 0.5) +
            geom_smooth(se = FALSE, method = lm, color = "red", na.rm = TRUE) +
          theme_bw() +
          geom_hline(aes(yintercept = value), data = limits_metric(metric_id) %>% enframe(), linetype = "dashed") +
          geom_vline(aes(xintercept = value), data = limits_metric(metric_id) %>% enframe(), linetype = "dashed") +
          annotate(
            "label",
            label = glue::glue("corr = {round(cor, 2)}"),
            x = first(limits) + diff(limits)*0.1,
            y = first(limits) + diff(limits)*0.9,
            hjust = 0,
            vjust = 1
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

    plot_scores$width <- 3 * 4
    plot_scores$height <- 3 * ceiling(length(plots) / 4)
    plot_scores$caption <- glue::glue("Score values of the same datasets (n = {length(unique(scores$dataset_id))}) in which cells were put either on the edges or on the milestones. Shown in the top left is the Spearman rank correlation.")

    lst(
      conformity = conformity %>% select(-cor),
      plot_scores
    )
  },
  plot_datasets = function(models) {
    # get identity dataset
    identities <- dataset_design %>%
      filter(topology_id == "bifurcation", num_cells == 100) %>%
      group_by(cell_positioning) %>%
      slice(1) %>%
      select(cell_positioning, dataset) %>%
      deframe() %>%
      map(invoke)

    # shuffle these datasets
    shuffleds <- map(identities, dynbenchmark:::perturb_shuffle_cells)

    # plot a continuous and grouped model
    grouping <- group_onto_nearest_milestones(identities[[1]])

    titles <- paste0("Cells on ", c("edges", "milestones", "edges", "milestones"), " \n", c("Reference", "Reference", "Shuffled cells", "Shuffled cells"))

    plot_datasets <- map2(
      c(identities, shuffleds),
      titles,
      function(model, title) {
        plot_dendro(model, grouping = grouping) +
          ggtitle(label_long(title)) +
          theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
      }) %>%
      patchwork::wrap_plots(ncol = 2)

    plot_datasets$width <- 3 * 4
    plot_datasets$height <- 6
    plot_datasets$caption <- "Example dataset in which cells are placed on the edges (left) or on the milestones (right), and with their original positions (top) or shuffled (bottom)."

    plot_datasets
  },
  observation = "All scores conform to this rule."
)


# combine rules
rules <- list(
  equal_identity,

  shuffle_cells_edgewise,
  shuffle_edges,
  shuffle_cells,
  combined_local_global_position_change,

  filter_cells,
  remove_divergence_regions,

  time_warping_start,
  time_warping_parabole,
  shuffle_lengths,

  move_cells_subedges,
  add_leaf_edges,
  add_connecting_edges,

  combined_position_topology,

  merge_bifurcation,
  combined_merge_bifurcation_shuffle_cells,
  concatenate_bifurcation,
  break_cycle,
  join_linear,
  split_linear,
  change_topology,

  cell_gathering

) %>% map(~list(.) %>% list_as_tibble()) %>% bind_rows()

# rules$assessment <- rules$assessment %>% map(function(x) {environment(x) <- new.env();x})

# rules <- rules %>% filter(id %in% c("cell_gathering"))

# check rules for contents
walkdf(rules, function(rule) {
  testthat::expect_true(all(
    c("id", "description", "crossing", "assessment") %in% names(rule)
  ))
})
