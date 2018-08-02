##  ............................................................................
##  Controls                                                                ####
perturb_identity <- function(dataset) dataset


##  ............................................................................
##  Changing cell ordering                                                  ####
## Switch cells
perturb_switch_n_cells <- function(dataset, switch_n = Inf) {
  switch_n <- min(switch_n, length(dataset$cell_ids))

  the_chosen_ones <- sample(dataset$cell_ids, switch_n)

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

perturb_switch_cells <- function(dataset, switch_perc = 1) {
    perturb_switch_n_cells(dataset, switch_n = length(dataset$cell_ids) * switch_perc)
}

## Switch edges
perturb_switch_n_edges <- function(dataset, switch_n = Inf) {
  if (nrow(dataset$divergence_regions)) {stop("To switch branches, dataset cannot have divergence regions")}

  switch_n = min(switch_n, nrow(dataset$milestone_network))

  # create an edge id and add it to the progressions
  milestone_network <- dataset$milestone_network %>%
    mutate(edge_id = row_number())

  progressions <- dataset$progressions %>%
    left_join(milestone_network, c("from", "to")) %>%
    select(-from, -to)

  # shuffle edges
  the_chosen_ones <- sample(milestone_network$edge_id, switch_n)
  mapper <- set_names(milestone_network$edge_id, milestone_network$edge_id)
  mapper[match(the_chosen_ones, mapper)] <- sample(the_chosen_ones)

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

perturb_switch_edges <- function(dataset, switch_perc = 1) {
  perturb_switch_n_edges(dataset, switch_n = nrow(dataset$milestone_network) * switch_perc)
}

##  ............................................................................
##  Chaning bifurcating trajectories                                        ####
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
perturb_break_cycles <- function(dataset) {
  if (dataset$trajectory_type != "directed_cycle") {stop("Need a cyclic dataset")}

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
  if(dataset$trajectory_type != "directed_linear") {stop("joining non-linear trajectories not supported")}
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

perturb_hairy <- function(dataset, nhairs = 10, sample_hair_length = function() 0.5) {
  milestone_network <- dataset$milestone_network
  progressions <- dataset$progressions

  new_milestone_i <- 1

  for (i in seq_len(nhairs)) {
    chosen_milestone_edge <- sample(seq_len(nrow(milestone_network)), 1, prob = milestone_network$length)
    chosen_edge <- as.list(milestone_network[chosen_milestone_edge, ])
    hair_length <- sample_hair_length()
    window_start <- runif(1, 0, 1-hair_length)
    window_end <- window_start + hair_length

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
        length = chosen_edge$length * c(window_start, window_end - window_start, 1-window_end),
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

## Extreme trajectories
# all cells except one at the beginning
perturb_extreme_beginning <- function() {}


## Remove cells
perturb_remove_cells <- function(dataset, retain_perc = 0.6) {
  retain_cell_ids <- sample(dataset$cell_ids, length(dataset$cell_ids) * retain_perc)
  progressions <- dataset$progressions %>% filter(cell_id %in% retain_cell_ids)

  dataset %>%
    add_trajectory(
      milestone_network = dataset$milestone_network,
      progressions = progressions,
      divergence_regions = dataset$divergence_regions
    )
}


## Add distant edge, chaning the topology but not the cell distances
perturb_add_leaf_edge <- function(dataset) {
  choosen_edge <- sample(intersect(dataset$milestone_network$from, dataset$milestone_network$to), 1)

  milestone_network <- bind_rows(
    dataset$milestone_network,
    tibble(from = choosen_edge, to = "END", length = sum(dataset$milestone_network$length)/2, directed = TRUE)
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
perturb_warp <- function(dataset) {
  progressions <- dataset$progressions %>% mutate(percentage = percentage^(2^runif(1, -2, 2)))
  milestone_network <- dataset$milestone_network %>% mutate(length = sample(length))

  dataset %>%
    add_trajectory(
      milestone_network = milestone_network,
      progressions = progressions,
      divergence_regions = dataset$divergence_regions
    )
}

# randomly change all lengths
perturb_shuffle_lengths <- function(dataset) {
  milestone_network <- dataset$milestone_network %>%
    mutate(length = runif(n()))

  dataset %>%
    add_trajectory(
      milestone_network = milestone_network,
      progressions = dataset$progressions,
      divergence_regions = dataset$divergence_regions
    )
}

##  ............................................................................
##  Combined perturbations                                                  ####
## Perturb topology and position
perturb_topology_and_position <- function(dataset) {
  dataset <- perturb_switch_all_cells(dataset)
  dataset <- perturb_add_distant_edge(dataset)
  dataset
}

## Change milestone network
change_network <- function(dataset, trajectory_type = "linear") {
  dataset$milestone_network <- dyntoy:::generate_milestone_network(trajectory_type)
  dataset$progressions <- dyntoy:::random_progressions(dataset$milestone_network, ncells = length(dataset$cell_ids))
  dataset$milestone_ids <- unique(c(dataset$milestone_network$from, dataset$milestone_network$to))

  recreate_dataset(dataset)
}

trajectory_models <- eval(formals(dyntoy:::generate_milestone_network)$model)

map(trajectory_models, function(x) function(dataset) change_network(dataset, x)) %>% setNames(paste0("perturb_change_network_", trajectory_models)) %>% list2env(.GlobalEnv)


rename_toy <- function(dataset, toy_id) {dataset$id<-toy_id;dataset}

# same dataset, but with the cell_percentages grouped at their maximal milestone, simulating the effect of a "marker-based" or "clustering-based" gold standard for real data
perturb_group_dataset <- function(dataset) {
  dataset$milestone_percentages <- dataset$milestone_percentages %>%
    group_by(cell_id) %>%
    mutate(percentage = ifelse(percentage == max(percentage), 1, 0)) %>%
    ungroup()

  dataset <- dynutils::wrap_ti_prediction(
    dataset$trajectory_type,
    dataset$id,
    dataset$cell_ids,
    dataset$milestone_ids,
    dataset$milestone_network,
    dataset$milestone_percentages
  )
  dataset$geodesic_dist <- dynutils:::compute_tented_geodesic_distances(dataset)
  dataset
}





## Simply the gold standard
perturbation_methods <- ls() %>% str_subset("^perturb_") %>% map(function(x) {
  id <- str_replace(x, "perturb_(.*)", "\\1")
  run_fun <- get(x)

  create_ti_method(id = id, run_fun = run_fun)()
})
