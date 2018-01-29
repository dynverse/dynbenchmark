k <- 10
n_perc <- 0.1
n_max <- 10

milestone_network$connectivity <- map2(
  as.character(milestone_network$from),
  as.character(milestone_network$to),
  function(from, to) {
    from_cell_ids <- cell_grouping %>% filter(group_id == from) %>% pull(cell_id)
    to_cell_ids <- cell_grouping %>% filter(group_id == to) %>% pull(cell_id)

    from_k <- floor(length(from_cell_ids) * n_perc) %>% max(1) %>% min(n_max)
    to_k <- floor(length(to_cell_ids) * n_perc) %>% max(1) %>% min(n_max)

    knn_distances <- FNN::knnx.dist(expression[from_cell_ids, ], expression[to_cell_ids, ], k=to_k)

    knn_distances %>%
      rowMeans() %>%
      sort() %>%
      head(from_k)
  }
)

milestones <- tibble(milestone_id=milestone_ids)
milestones$connectivity <- map(milestones$milestone_id, function(milestone_id) {
  cell_ids <- cell_grouping %>% filter(group_id == milestone_id) %>% pull(cell_id)
  k <- floor(length(cell_ids) * n_perc) %>% max(1) %>% min(n_max)

  knn_distances <- FNN::knn.dist(expression[cell_ids, ], k=k)

  knn_distances %>% rowMeans()
}) %>% set_names(milestone_ids)


milestone_network %>% mutate(inter=TRUE) %>%
  bind_rows(milestones %>% mutate(inter=FALSE)) %>% unnest(connectivity) %>%
  ggplot() + ggbeeswarm::geom_beeswarm(aes(inter, connectivity, color=milestone_id))
