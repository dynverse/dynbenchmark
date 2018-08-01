## Again gold standard
perturb_gs <- function(dataset) {
  dataset
}

## Switch cells
perturb_switch_n_cells <- function(dataset, n = length(dataset$cell_ids)) {
  the_chosen_ones <- sample(dataset$cell_ids, n)

  mapper <- set_names(dataset$cell_ids, dataset$cell_ids)
  mapper[match(the_chosen_ones, mapper)] <- rev(the_chosen_ones)

  dataset$progressions$cell_id <- mapper[dataset$progressions$cell_id]

  dataset
}

perturb_switch_two_cells <- function(dataset) perturb_switch_n_cells(dataset, 2)

perturb_switch_all_cells <- function(dataset) perturb_switch_n_cells(dataset, length(unique(dataset$progressions$cell_id)))

percs <- seq(0, 1, 0.1)
perc_perturbators <- map(percs, function(perc) {function(dataset) perturb_switch_n_cells(dataset, length(dataset$cell_id) * perc)}) %>%
  set_names(paste0("perturb_switch_", percs * 100))
perc_perturbators %>% list2env(.GlobalEnv)


# Break cycle
# this should be easier, no?
perturb_break_cycles <- function(dataset) {
  # dataset <- generate_cycle()

  net <- dataset$milestone_network %>% mutate(linkid = seq_len(n()))
  visited <- c()
  remove_links <- c()

  walker <- function(net, curmilestone, curlinkid) {
    if(curmilestone %in% visited) {
      remove_links <<- c(remove_links, curlinkid)
    } else {
      visited <<- c(visited, curmilestone)
      net %>%
        filter(from == curmilestone) %>%
        {walk2(.$to, .$linkid, function(to, linkid) {walker(net, to, linkid)})}
    }
  }

  walker(net, net$from[[1]], NULL)

  new_milestone_n <- 1
  for(remove_link in remove_links) {
    from <- dataset$milestone_network[remove_link, ]$from
    to <- dataset$milestone_network[remove_link, ]$to
    length <- dataset$milestone_network[remove_link, ]$length

    newto <- paste0("NM", new_milestone_n)
    new_milestone_n <- new_milestone_n + 1

    dataset$progressions[(dataset$progressions$from == from) & (dataset$progressions$to == to),]$to = newto
    dataset$milestone_network <- dataset$milestone_network %>% add_row(from = from, to = newto, length = length)

    dataset$milestone_ids <- c(dataset$milestone_ids, newto)
  }

  dataset$milestone_network <- dataset$milestone_network[-remove_links, ]

  recreate_dataset(dataset)
}

# Join linear
perturb_join_linear <- function(dataset) {
  # dataset <- generate_linear()
  if(nrow(dataset$milestone_network) != 1) {stop("joining non-linear trajectories not supported")}

  length <- dataset$milestone_network$length

  dataset$milestone_network <- tibble::tribble(
    ~from, ~to,
    "M1", "M2",
    "M2", "M3",
    "M3", "M1"
  ) %>% mutate(length = length/3, directed = TRUE)

  dataset$milestone_ids <- c("M1", "M2", "M3")

  dataset$progressions <- dataset$progressions %>%
    mutate(
      from = dataset$milestone_ids[as.numeric(percentage*3) + 1],
      to = dataset$milestone_ids[((as.numeric(percentage*3) +1) %% 3 + 1)],
      percentage = (percentage * 3) %% 1
    )

  recreate_dataset(dataset)
}


# Split linear to bifurcation
perturb_split_linear <- function(dataset) {
  # dataset <- generate_linear()
  if(nrow(dataset$milestone_network) != 1) {stop("joining non-linear trajectories not supported")}

  length <- dataset$milestone_network$length

  dataset$milestone_network <- tibble::tribble(
    ~from, ~to,
    "M1", "M2",
    "M2", "M3",
    "M2", "M4"
  ) %>% mutate(length = length/2, directed = TRUE)

  dataset$milestone_ids <- c("M1", "M2", "M3", "M4")

  dataset$progressions <- dataset$progressions %>%
    mutate(
      from = ifelse(percentage > 0.5, "M2", "M1"),
      to = ifelse(percentage > 0.5, sample(c("M3", "M4"), n(), replace = TRUE), "M2"),
      percentage = (percentage * 2) %% 1
    )
  recreate_dataset(dataset)
}


# Make a trajectory hairy
#dataset <- generate_linear()

perturb_hairy <- function(dataset, nhairs = 10, overall_hair_length = 1) {
  if(overall_hair_length < 1) {stop("hair length should be larger than 1")}

  newmilestone_network <- dataset$milestone_network
  newprogressions <- dataset$progressions

  new_milestone_i <- 1

  for (i in seq_len(nhairs)) {
    chosen_milestone_edge <- sample(seq_len(nrow(newmilestone_network)), 1, prob = newmilestone_network$length)
    chosen_edge <- as.list(newmilestone_network[chosen_milestone_edge, ])
    hair_length <- runif(1, 0.1, 0.2)
    window_start <- runif(1, 0, 1-hair_length)
    window_end <- window_start + hair_length

    new_from <- paste0("NM_", new_milestone_i)
    new_to <- paste0("NM_", new_milestone_i + 1)
    new_milestone_i <- new_milestone_i + 2

    subset_window <- (
      newprogressions$from == chosen_edge$from &
        newprogressions$to == chosen_edge$to &
        newprogressions$percentage >= window_start &
        newprogressions$percentage <= window_end
    )
    newprogressions[subset_window, ] <- newprogressions[subset_window, ] %>%
      mutate(
        from = new_from,
        to = new_to,
        percentage = ((percentage - min(percentage)) / (window_end - window_start)) / overall_hair_length
      )

    subset_before <- (
      newprogressions$from == chosen_edge$from &
        newprogressions$to == chosen_edge$to &
        newprogressions$percentage < window_start
    )

    newprogressions[subset_before, ] <- newprogressions[subset_before, ] %>%
      mutate(
        from = from,
        to = new_from,
        percentage = percentage / window_start
      )


    subset_after <- (
      newprogressions$from == chosen_edge$from &
        newprogressions$to == chosen_edge$to &
        newprogressions$percentage > window_end
    )

    newprogressions[subset_after, ] <- newprogressions[subset_after, ] %>%
      mutate(
        from = new_from,
        to = to,
        percentage = (percentage - window_end) / (1 - window_end)
      )

    newmilestone_network <- newmilestone_network %>%
      filter(!(from == chosen_edge$from & to == chosen_edge$to)) %>%
      bind_rows(tibble(
        from = c(chosen_edge$from, new_from, new_from),
        to = c(new_from, new_to, chosen_edge$to),
        length = chosen_edge$length * c(window_start, window_end - window_start, 1-window_end) * overall_hair_length
      ))
  }

  newdataset <- dynutils::wrap_ti_prediction(
    dataset$trajectory_type,
    dataset$id,
    dataset$cell_ids,
    unique(c(newmilestone_network$from, newmilestone_network$to)),
    newmilestone_network,
    progressions = newprogressions
  )

  newdataset$geodesic_dist <- dynutils:::compute_tented_geodesic_distances(newdataset)

  newdataset
}

perturb_hairy_small <- function(dataset) {perturb_hairy(dataset, nhairs = 2)}
perturb_hairy_large <- function(dataset) {perturb_hairy(dataset, nhairs = 20)}
# perturb_hairy_long <- function(dataset) {perturb_hairy(dataset, overall_hair_length = 2)}

# Warping the times
# very quick and dirty way to wrap, but it works :p
perturb_warp <- function(dataset) {
  # dataset <- generate_linear()

  dataset$progressions$percentage <- dataset$progressions$percentage^(2^runif(1, -2, 2))

  recreate_dataset(dataset)
}

## Extreme trajectories
# all cells except one at the beginning
perturb_extreme_beginning <- function() {}


## Remove cells
perturb_remove_cells <- function(dataset) {
  allcells <- dataset$cell_ids
  retained_cells <- sample(allcells, length(allcells) * 0.6)
  dataset$progressions <- dataset$progressions %>% filter(cell_id %in% retained_cells)

  recreate_dataset(dataset)
}


## Add distant edge, chaning the structure but not the cell distances
perturb_add_distant_edge <- function(dataset) {
  dataset$milestone_network <- bind_rows(
    dataset$milestone_network,
    tibble(from = c(dataset$milestone_ids[[1]], "NEWM"), to = c("NEWM", dataset$milestone_ids[[2]]), length = sum(dataset$milestone_network$length)/2, directed = TRUE)
  )
  dataset$milestone_ids <- c(dataset$milestone_ids, "NEWM")
  recreate_dataset(dataset)
}

## Change lengths
# randomly change all lengths
perturb_change_lengths <- function(dataset) {
  dataset$milestone_network <- dataset$milestone_network %>%
    mutate(length = runif(n()))
  recreate_dataset(dataset)
}

# change lengths of only terminal edges, while keeping the distances constant
perturb_change_terminal_lengths <- function(dataset) {
  dataset$milestone_network <- dataset$milestone_network %>%
    mutate(length = ifelse(to %in% from, length, length*2))
  dataset$progressions <- dataset$progressions %>%
    mutate(percentage = ifelse(to %in% from, percentage, percentage/2))
  recreate_dataset(dataset)
}


## Perturb structure and position
perturb_structure_and_position <- function(dataset) {
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
group_dataset <- function(dataset) {
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
