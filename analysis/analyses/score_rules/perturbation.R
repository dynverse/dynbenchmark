## Again gold standard
perturb_gs <- function(task) {
  task
}

# Switch cells
perturb_switch_n_cells <- function(task, n=length(task$cell_ids)) {
  the_chosen_ones <- sample(task$cell_ids, n)

  mapper <- set_names(task$cell_ids, task$cell_ids)
  mapper[match(the_chosen_ones, mapper)] <- rev(the_chosen_ones)

  task$milestone_percentages$cell_id <- mapper[task$milestone_percentages$cell_id]
  task$progression$cell_id <- mapper[task$progression$cell_id]

  recreate_task(task)
}

perturb_switch_two_cells <- function(task) perturb_switch_n_cells(task, 2)

perturb_switch_all_cells <- function(task) perturb_switch_n_cells(task, length(unique(task$progressions$cell_id)))

percs <- seq(0, 1, 0.1)
perc_perturbators <- map(percs, function(perc) {function(task) perturb_switch_n_cells(task, length(task$cell_id) * perc)}) %>%
  set_names(paste0("perturb_switch_", percs * 100))
perc_perturbators %>% list2env(.GlobalEnv)


# Break cycle
# this should be easier, no?
perturb_break_cycles <- function(task) {
  # task <- generate_cycle()

  net <- task$milestone_network %>% mutate(linkid = seq_len(n()))
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
    from <- task$milestone_network[remove_link, ]$from
    to <- task$milestone_network[remove_link, ]$to
    length <- task$milestone_network[remove_link, ]$length

    newto <- paste0("NM", new_milestone_n)
    new_milestone_n <- new_milestone_n + 1

    task$progressions[(task$progressions$from == from) & (task$progressions$to == to),]$to = newto
    task$milestone_network <- task$milestone_network %>% add_row(from=from, to=newto, length=length)

    task$milestone_ids <- c(task$milestone_ids, newto)
  }

  task$milestone_network <- task$milestone_network[-remove_links, ]

  recreate_task(task)
}

# Join linear
perturb_join_linear <- function(task) {
  # task <- generate_linear()
  if(nrow(task$milestone_network) != 1) {stop("joining non-linear trajectories not supported")}

  length <- task$milestone_network$length

  task$milestone_network <- tibble::tribble(
    ~from, ~to,
    "M1", "M2",
    "M2", "M3",
    "M3", "M1"
  ) %>% mutate(length=length/3, directed=TRUE)

  task$milestone_ids <- c("M1", "M2", "M3")

  task$progressions <- task$progressions %>%
    mutate(
      from = task$milestone_ids[as.numeric(percentage*3) + 1],
      to = task$milestone_ids[((as.numeric(percentage*3) +1) %% 3 + 1)],
      percentage = (percentage * 3) %% 1
    )

  recreate_task(task)
}


# Split linear to bifurcation
perturb_split_linear <- function(task) {
  # task <- generate_linear()
  if(nrow(task$milestone_network) != 1) {stop("joining non-linear trajectories not supported")}

  length <- task$milestone_network$length

  task$milestone_network <- tibble::tribble(
    ~from, ~to,
    "M1", "M2",
    "M2", "M3",
    "M2", "M4"
  ) %>% mutate(length=length/2, directed=TRUE)

  task$milestone_ids <- c("M1", "M2", "M3", "M4")

  task$progressions <- task$progressions %>%
    mutate(
      from = ifelse(percentage > 0.5, "M2", "M1"),
      to = ifelse(percentage > 0.5, sample(c("M3", "M4"), n(), replace=TRUE), "M2"),
      percentage = (percentage * 2) %% 1
    )
  recreate_task(task)
}


# Make a trajectory hairy
#task <- generate_linear()

perturb_hairy <- function(task, nhairs=10) {
  newmilestone_network <- task$milestone_network
  newprogressions <- task$progressions

  new_milestone_i <- 1

  for (i in seq_len(nhairs)) {
    chosen_milestone_edge <- sample(seq_len(nrow(newmilestone_network)), 1, prob=newmilestone_network$length)
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
        percentage = (percentage - min(percentage)) / (window_end - window_start)
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
        length = chosen_edge$length * c(window_start, window_end - window_start, 1-window_end)
      ))
  }

  newtask <- wrap_ti_prediction(
    task$ti_type,
    task$id,
    task$cell_ids,
    unique(c(newmilestone_network$from, newmilestone_network$to)),
    newmilestone_network,
    progressions = newprogressions
  )

  newtask$geodesic_dist <- compute_emlike_dist(newtask)

  #dyneval::plot_default(task)
  #dyneval::plot_default(newtask)

  newtask
}

perturb_hairy_small <- function(task) {perturb_hairy(task, nhairs=2)}
perturb_hairy_large <- function(task) {perturb_hairy(task, nhairs=20)}

# Warping the times
# very quick and dirty way to wrap, but it works :p
perturb_warp <- function(task) {
  # task <- generate_linear()

  task$progressions$percentage <- task$progressions$percentage^(2^runif(1, -2, 2))

  recreate_task(task)
}

## Extreme trajectories
# all cells except one at the beginning
perturb_extreme_beginning <- function() {}


## Remove cells
perturb_remove_cells <- function(task) {
  allcells <- task$cell_ids
  retained_cells <- sample(allcells, length(allcells) * 0.6)
  task$progressions <- task$progressions %>% filter(cell_id %in% retained_cells)

  recreate_task(task)
}


## Add distant edge, chaning the structure but not the cell distances
perturb_add_distant_edge <- function(task) {
  task$milestone_network <- bind_rows(
    task$milestone_network,
    tibble(from=c(task$milestone_ids[[1]], "NEWM"), to=c("NEWM", task$milestone_ids[[2]]), length=sum(task$milestone_network$length)/2, directed=TRUE)
  )
  task$milestone_ids <- c(task$milestone_ids, "NEWM")
  recreate_task(task)
}


### Some helper functions-------------------

# Recreate task, forcing a reculaculation of geodesic distances
recreate_task <- function(task) {
  task <- wrap_ti_prediction(
    task$ti_type,
    task$id,
    task$cell_ids,
    task$milestone_ids,
    task$milestone_network,
    progression=task$progression
  )
  task$geodesic_dist <- compute_emlike_dist(task)

  task
}


rename_toy <- function(task, toy_id) {task$id<-toy_id;task}
