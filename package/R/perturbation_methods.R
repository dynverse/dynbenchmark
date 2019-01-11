##  ............................................................................
##  Controls                                                                ####
perturb_identity <- function(dataset) {
  dataset
}


##  ............................................................................
##  Changing cell ordering                                                  ####
## Shuffle cells
perturb_shuffle_n_cells <- function(dataset, shuffle_n = Inf, seed = NA) {
  if (is.numeric(seed) && !is.na(seed)) set.seed(seed)

  shuffle_n <- min(shuffle_n, length(dataset$cell_ids))

  the_chosen_ones <- sample(dataset$cell_ids, shuffle_n)

  mapper <- set_names(dataset$cell_ids, dataset$cell_ids)
  mapper[match(the_chosen_ones, mapper)] <- sample(the_chosen_ones)

  dataset$progressions$cell_id <- mapper[dataset$progressions$cell_id]

  dataset %>%
    add_trajectory(
      milestone_network = dataset$milestone_network,
      progressions = dataset$progressions,
      divergence_regions = dataset$divergence_regions
    )
}

perturb_shuffle_cells <- function(dataset, shuffle_perc = 1, seed = NA) {
  # source(scripts_file("helper-perturbations.R", experiment_id = "02-metrics/02-metric_conformity"))

  perturb_shuffle_n_cells(dataset, shuffle_n = length(dataset$cell_ids) * shuffle_perc, seed = seed)
}

## Shuffle edges
perturb_shuffle_n_edges <- function(dataset, shuffle_n = Inf, seed = NA) {
  if (!is.null(seed)) set.seed(seed)
  if (nrow(dataset$divergence_regions)) {stop("To shuffle branches, dataset cannot have divergence regions")}

  shuffle_n = min(shuffle_n, nrow(dataset$milestone_network))

  if (shuffle_n == 1) {
    dataset
  } else {
    # create an edge id and add it to the progressions
    milestone_network <- dataset$milestone_network %>%
      mutate(edge_id = row_number())

    progressions <- dataset$progressions %>%
      left_join(milestone_network %>% select(from, to, edge_id), c("from", "to")) %>%
      select(-from, -to)

    # shuffle edges
    the_chosen_ones <- sample(milestone_network$edge_id, shuffle_n)
    mapper <- set_names(milestone_network$edge_id, milestone_network$edge_id)
    mapper[match(the_chosen_ones, mapper)] <- sample(the_chosen_ones, size = length(the_chosen_ones))

    # change in milestone_network and join with progressions
    milestone_network$edge_id <- mapper[as.character(milestone_network$edge_id)]

    progressions <- left_join(progressions, milestone_network, "edge_id") %>%
      select(cell_id, from, to, percentage)

    milestone_network <- milestone_network %>% select(-edge_id)

    dataset %>%
      add_trajectory(
        milestone_network = milestone_network,
        progressions = progressions,
        divergence_regions = dataset$divergence_regions
      )
  }
}

perturb_shuffle_edges <- function(dataset, shuffle_perc = 1, seed = NA) {
  # source(scripts_file("helper-perturbations.R", experiment_id = "02-metrics/02-metric_conformity"))

  perturb_shuffle_n_edges(dataset, shuffle_n = round(nrow(dataset$milestone_network) * shuffle_perc), seed = seed)
}


## Remove cells
perturb_filter_cells <- function(dataset, filter_perc = 0.6, seed = NA) {
  if (!is.null(seed)) set.seed(seed)

  filter_cell_ids <- sample(dataset$cell_ids, length(dataset$cell_ids) * filter_perc)
  progressions <- dataset$progressions %>% filter(!(cell_id %in% filter_cell_ids))

  dataset %>%
    add_trajectory(
      milestone_network = dataset$milestone_network,
      progressions = progressions,
      divergence_regions = dataset$divergence_regions
    )
}


## Shuffle within edge
perturb_shuffle_cells_edgewise <- function(dataset, seed = NA) {
  if (!is.null(seed)) set.seed(seed)

  progressions <- dataset$progressions %>% mutate(percentage = runif(n()))

  dataset %>%
    add_trajectory(
      milestone_network = dataset$milestone_network,
      progressions = progressions,
      divergence_regions = dataset$divergence_regions
    )
}


## Remove cells
perturb_remove_divergence_regions <- function(dataset) {
  progressions <- dataset$progressions %>% group_by(cell_id) %>% arrange(-percentage) %>% slice(1) %>% ungroup()
  divergence_regions <- dataset$divergence_regions %>% filter(FALSE)

  dataset %>%
    add_trajectory(
      milestone_network = dataset$milestone_network,
      progressions = progressions,
      divergence_regions = divergence_regions
    )
}

#   ____________________________________________________________________________
#   Changing the topology                                                   ####
##  ............................................................................
##  Changing bifurcating trajectories                                        ####
# Merge the branch after bifurcations
perturb_merge_bifurcation <- function(dataset) {
  if (nrow(dataset$milestone_network) != 3) {
    stop("Merge bifurcations requires a bifurcating dataset with three milestone edges")
  }
  if (nrow(dataset$divergence_regions) > 0) {
    stop("Dataset cannot contain divergence regions when merging bifurcations")
  }

  to_milestones <- dataset$milestone_network %>% group_by(from) %>% filter(n() == 2) %>% pull(to)

  milestone_network <- dataset$milestone_network %>%
    mutate(to = ifelse(to %in% to_milestones, "END", to)) %>%
    group_by(from, to) %>%
    filter(row_number() == 1) %>%
    ungroup()
  progressions <- dataset$progressions %>% mutate(to = ifelse(to %in% to_milestones, "END", to))

  dataset %>%
    add_trajectory(
      milestone_network = milestone_network,
      progressions = progressions
    )
}

# Put one of the bifurcation edges at the end of the other bifurcation edge
perturb_concatenate_bifurcation <- function(dataset) {
  if (nrow(dataset$milestone_network) != 3) {
    stop("Concatenating bifurcations requires a bifurcating dataset with three milestone edges")
  }
  if (nrow(dataset$divergence_regions) > 0) {
    stop("Dataset cannot contain divergence regions")
  }

  to_milestones <- dataset$milestone_network %>% group_by(from) %>% filter(n() == 2) %>% pull(to)

  milestone_network <- dataset$milestone_network %>%
    mutate(from = ifelse(to == to_milestones[[2]], to_milestones[[1]], from))
  progressions <- dataset$progressions %>%
    mutate(from = ifelse(to == to_milestones[[2]], to_milestones[[1]], from))

  dataset %>%
    add_trajectory(
      milestone_network = milestone_network,
      progressions = progressions
    )
}


##  ............................................................................
##  Changing cyclic trajectories                                            ####
perturb_break_cycle <- function(dataset) {
  if (dataset$trajectory_type != "cycle") {stop("Need a cyclic dataset")}

  unlink_milestone <- sample(dataset$milestone_ids, 1)

  milestone_network <- dataset$milestone_network %>% mutate(to = ifelse(to == unlink_milestone, "END", to))
  progressions <- dataset$progressions %>% mutate(to = ifelse(to == unlink_milestone, "END", to))

  dataset %>%
    add_trajectory(
      milestone_network = milestone_network,
      progressions = progressions
    )
}


##  ............................................................................
##  Changing linear trajectories                                            ####
perturb_join_linear <- function(dataset) {
  if(dataset$trajectory_type != "linear") {stop("joining non-linear trajectories not supported")}
  if(nrow(dataset$milestone_network) < 3) {stop("Need at least 3 edges in the linear dataset to be able to join")}

  start_milestone_id <- setdiff(dataset$milestone_network$from, dataset$milestone_network$to)
  end_milestone_id <- setdiff(dataset$milestone_network$to, dataset$milestone_network$from)

  milestone_network <- dataset$milestone_network %>% mutate(to = ifelse(to == end_milestone_id, start_milestone_id, to))
  progressions <- dataset$progressions %>% mutate(to = ifelse(to == end_milestone_id, start_milestone_id, to))

  dataset %>%
    add_trajectory(
      milestone_network = milestone_network,
      progressions = progressions
    )
}


# Split linear to bifurcation
perturb_move_terminal_branch <- function(dataset) {
  terminal_milestone <- sample(setdiff(dataset$milestone_network$to, dataset$milestone_network$from), 1)

  intermediate_milestones <- intersect(dataset$milestone_network$from, dataset$milestone_network$to)
  possible_milestones <- dataset$milestone_network %>%
    filter(
      from %in% intermediate_milestones,
      to != terminal_milestone
    ) %>%
    pull(from)
  if (length(possible_milestones) == 0) {stop("No possible intermediate milestone on which the branch can be put")}
  selected_milestone <- sample(possible_milestones, 1)

  milestone_network <- dataset$milestone_network %>% mutate(from = ifelse(to == terminal_milestone, selected_milestone, from))
  progressions <- dataset$progressions %>% mutate(from = ifelse(to == terminal_milestone, selected_milestone, from))

  dataset %>%
    add_trajectory(
      milestone_network = milestone_network,
      progressions = progressions
    )
}


##  ............................................................................
##  Adding extra edges to the topology                                      ####
#dataset <- generate_linear()
perturb_move_cells_subedges <- function(
  dataset,
  n_edges = 1,
  sample_subedge_length_lower = 0.2,
  sample_subedge_length_higher = 0.4,
  sample_subedge_length = function() runif(1, sample_subedge_length_lower, sample_subedge_length_higher),
  subedge_length_magnification = 1
) {
  milestone_network <- dataset$milestone_network
  progressions <- dataset$progressions

  new_milestone_i <- 1

  for (i in seq_len(n_edges)) {
    chosen_milestone_edge <- sample(seq_len(nrow(milestone_network)), 1, prob = milestone_network$length)
    chosen_edge <- as.list(milestone_network[chosen_milestone_edge, ])
    subedge_length <- sample_subedge_length()

    if (subedge_length == 0 || subedge_length_magnification == 0) next

    window_start <- runif(1, 0, 1-subedge_length)
    window_end <- window_start + subedge_length

    new_from <- paste0("NI_", new_milestone_i)
    new_to <- paste0("NT_", new_milestone_i)
    new_milestone_i <- new_milestone_i + 1

    subset_window <- (
      progressions$from == chosen_edge$from &
        progressions$to == chosen_edge$to &
        progressions$percentage >= window_start &
        progressions$percentage <= window_end
    )
    progressions[subset_window, ] <- progressions[subset_window, ] %>%
      mutate(
        from = new_from,
        to = new_to,
        percentage = ((percentage - min(percentage)) / (window_end - window_start))
      )

    subset_before <- (
      progressions$from == chosen_edge$from &
        progressions$to == chosen_edge$to &
        progressions$percentage < window_start
    )

    progressions[subset_before, ] <- progressions[subset_before, ] %>%
      mutate(
        from = from,
        to = new_from,
        percentage = percentage / window_start
      )

    subset_after <- (
      progressions$from == chosen_edge$from &
        progressions$to == chosen_edge$to &
        progressions$percentage > window_end
    )

    progressions[subset_after, ] <- progressions[subset_after, ] %>%
      mutate(
        from = new_from,
        to = to,
        percentage = (percentage - window_end) / (1 - window_end)
      )

    milestone_network <- milestone_network %>%
      filter(!(from == chosen_edge$from & to == chosen_edge$to)) %>%
      bind_rows(tibble(
        from = c(chosen_edge$from, new_from, new_from),
        to = c(new_from, new_to, chosen_edge$to),
        length = chosen_edge$length * c(window_start, window_end - window_start, 1-window_end) * subedge_length_magnification,
        directed = TRUE
      ))
  }

  dataset %>%
    add_trajectory(
      milestone_network = milestone_network,
      progressions = progressions,
      divergence_regions = dataset$divergence_regions
    )
}

## Add edges, changing the topology but not the cell distances
perturb_add_intermediate_edges <- function(dataset, n_edges = 1) {
  # select milestones with at least one from, if the n_edges parameter is too large, cap it at the number of edges
  choosen_milestones <- sample(unique(dataset$milestone_network$from), n_edges, replace = TRUE) %>% unique()
  n_edges <- length(choosen_milestones)

  milestone_network <- bind_rows(
    dataset$milestone_network %>%
      mutate(
        from = ifelse(from %in% choosen_milestones, paste0("INT_", from), from),
        to = ifelse(from %in% choosen_milestones, paste0("INT_", from), to)
      ),
    tibble(
      from = paste0(choosen_milestones),
      to = paste0("INT_", choosen_milestones),
      length = sample(dataset$milestone_network$length, length(choosen_milestones), replace = TRUE),
      directed = TRUE
    )
  )

  progressions <- dataset$progressions %>%
    mutate(
      from = ifelse(from %in% choosen_milestones, paste0("INT_", from), from),
      to = ifelse(from %in% choosen_milestones, paste0("INT_", from), to)
    )

  dataset %>%
    add_trajectory(
      milestone_network = milestone_network,
      progressions = progressions,
      divergence_regions = dataset$divergence_regions
    )
}


perturb_add_leaf_edges <- function(dataset, n_edges = 1) {
  choosen_froms <- sample(dataset$milestone_ids, n_edges, replace = TRUE)

  milestone_network <- bind_rows(
    dataset$milestone_network,
    tibble(
      from = choosen_froms,
      to = paste0("END_", seq_len(n_edges)),
      length = sample(dataset$milestone_network$length, n_edges, replace = TRUE),
      directed = TRUE
    )
  )

  dataset %>%
    add_trajectory(
      milestone_network = milestone_network,
      progressions = dataset$progressions,
      divergence_regions = dataset$divergence_regions
    )
}

perturb_add_connecting_edges <- function(dataset, n_edges = 1) {
  # select n_edges combinations which are not yet in dataset$milestone_network (also not in reverse direction!)
  possible_edges <- tidyr::complete(
    bind_rows(
      dataset$milestone_network,
      dataset$milestone_network %>% rename(from = to, to = from)
    ),
    from,
    to
  ) %>%
    filter(is.na(length)) %>%
    filter(from != to)
  n_edges <- min(nrow(possible_edges), n_edges)

  new_edges <- bind_rows(
    possible_edges %>% mutate(to = paste0("INT_", row_number())),
    possible_edges %>% mutate(from = paste0("INT_", row_number()))
  ) %>%
    mutate(
      directed = TRUE,
      length = sample(dataset$milestone_network$length, n(), replace = TRUE)
    )

  milestone_network <- bind_rows(
    dataset$milestone_network,
    possible_edges %>%
      sample_n(n_edges) %>%
      mutate(
        directed = TRUE,
        length = sample(dataset$milestone_network$length, n(), replace = TRUE)
      )
  )

  dataset %>%
    add_trajectory(
      milestone_network = milestone_network,
      progressions = dataset$progressions,
      divergence_regions = dataset$divergence_regions
    )
}

##  ............................................................................
##  Warping the times                                                       ####
# warping the cells within an edge
perturb_time_warping_start <- function(dataset, warp_magnitude = 1) {
  progressions <- dataset$progressions %>% mutate(percentage = percentage^(exp(runif(n(), -warp_magnitude, warp_magnitude))))

  dataset %>%
    add_trajectory(
      milestone_network = dataset$milestone_network,
      progressions = progressions,
      divergence_regions = dataset$divergence_regions
    )
}

# move cells closer to the end or beginning of the edge
perturb_time_warping_parabole <- function(dataset, warp_magnitude = 1) {
  progressions <- dataset$progressions %>% mutate(percentage = ((abs(percentage - 0.5) * 2)^(1/warp_magnitude)) * sign(percentage - 0.5) / 2 + 0.5)

  dataset %>%
    add_trajectory(
      milestone_network = dataset$milestone_network,
      progressions = progressions,
      divergence_regions = dataset$divergence_regions
    )
}

# randomly change all lengths
perturb_shuffle_lengths <- function(dataset) {
  milestone_network <- dataset$milestone_network %>%
    mutate(length = length[sample.int(length(length))])

  dataset %>%
    add_trajectory(
      milestone_network = milestone_network,
      progressions = dataset$progressions,
      divergence_regions = dataset$divergence_regions
    )
}


##  ............................................................................
##  Direct topological changes                                              ####
perturb_change_topology <- function(dataset, topology_id = "linear") {
  if (nrow(dataset$milestone_network) != 5) {stop("Can only change topology if there are 5 edges")}
  if (nrow(dataset$divergence_regions)) {stop("To change the topology, dataset cannot have divergence regions")}

  # create an edge id and add it to the progressions
  milestone_network <- dataset$milestone_network %>%
    mutate(edge_id = row_number())

  progressions <- dataset$progressions %>%
    left_join(milestone_network %>% select(from, to, edge_id), c("from", "to"))

  # now remove the from and to from milestone_network and progressions
  milestone_network <- milestone_network %>%
    select(-from, -to) %>%
    bind_cols(dynbenchmark:::topologies_with_same_n_milestones[[topology_id]])

  progressions <- progressions %>%
    select(-from, -to) %>%
    left_join(milestone_network %>% select(edge_id, from, to), "edge_id") %>%
    select(-edge_id)

  milestone_network <- milestone_network %>% select(-edge_id)

  dataset %>%
    add_trajectory(
      milestone_network = milestone_network,
      progressions = progressions,
      divergence_regions = dataset$divergence_regions
    )
}


##  ............................................................................
##  Combined perturbations                                                  ####
perturb_shuffle_cells_and_add_connecting_edges <- function(dataset, shuffle_perc = 0.2, n_edges = 1) {
  dataset <- perturb_add_connecting_edges(dataset, n_edges)
  dataset <- perturb_shuffle_cells(dataset, shuffle_perc)

  dataset
}

perturb_shuffle_cells_and_merge_bifurcation <- function(dataset, shuffle_perc = 0.2) {
  dataset <- perturb_merge_bifurcation(dataset)
  dataset <- perturb_shuffle_cells(dataset, shuffle_perc)

  dataset
}

# perturb_function_names <-
#   readr::read_lines("package/R/perturbation_methods.R") %>%
#   stringr::str_subset("^(perturb_[^ ]*) *<- *function.*$") %>%
#   stringr::str_replace("^(perturb_[^ ]*) *<- *function.*$", "\\1")
# paste(perturb_function_names, collapse = ",\n  ") %>% cat

#' @importFrom tibble lst
perturbation_methods <- tibble::lst(
  perturb_identity,
  perturb_shuffle_n_cells,
  perturb_shuffle_cells,
  perturb_shuffle_n_edges,
  perturb_shuffle_edges,
  perturb_filter_cells,
  perturb_shuffle_cells_edgewise,
  perturb_remove_divergence_regions,
  perturb_merge_bifurcation,
  perturb_concatenate_bifurcation,
  perturb_break_cycle,
  perturb_join_linear,
  perturb_move_terminal_branch,
  perturb_move_cells_subedges,
  perturb_add_intermediate_edges,
  perturb_add_leaf_edges,
  perturb_add_connecting_edges,
  perturb_time_warping_start,
  perturb_time_warping_parabole,
  perturb_shuffle_lengths,
  perturb_change_topology,
  perturb_shuffle_cells_and_add_connecting_edges,
  perturb_shuffle_cells_and_merge_bifurcation
)


#' @importFrom dynwrap create_ti_method_r
#' @include suite_benchmark_generate_design.R
#' @include suite_benchmark_submit.R
perturbation_methods_design <-
  map(names(perturbation_methods), function(name) {
    run_fun <- perturbation_methods[[name]]

    if (!"verbose" %in% formalArgs(run_fun)) formals(run_fun)$verbose <- FALSE
    if (!"seed" %in% formalArgs(run_fun)) formals(run_fun)$seed <- NA

    param_names <- setdiff(formalArgs(run_fun), c("dataset", "seed", "verbose"))

    dynwrap::create_ti_method_r(
      id = name %>% gsub("^perturb_", "", .),
      run_fun = run_fun,
      parameters = NULL,
      package_loaded = c("dplyr", "purrr", "dynwrap", "dynbenchmark"),
      input_required = "dataset",
      input_optional = NULL,
      output = "trajectory",
      type = "control",
      return_function = FALSE
    )

  }) %>%
  dynbenchmark:::process_methods_design()
