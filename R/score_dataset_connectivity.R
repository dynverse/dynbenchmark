#' Score milestone connectivity
#' @param counts counts matrix
#' @param cell_grouping data frame contain cell groups
#' @param milestone_ids vector of all milestones
#' @param milestone_network data frame contain the milestone network
#' @param perc Percentage of lowest distances to use between groups
#'
#' @importFrom reshape2 melt
#'
#' @export
score_milestone_connectivity <- function(counts, cell_grouping, milestone_ids, milestone_network, perc=0.1) {
  distances <- dynutils::correlation_distance(counts)

  alldistances <- distances %>%
    reshape2::melt(varnames=c("from_cell_id", "to_cell_id"), value.name="distance") %>%
    mutate_if(is.factor, funs(as.character)) %>%
    left_join(cell_grouping %>% rename(from = "group_id"), by=c("from_cell_id" = "cell_id")) %>%
    left_join(cell_grouping %>% rename(to = "group_id"), by=c("to_cell_id" = "cell_id"))

  distance_ecdf <- ecdf(alldistances$distance)

  connectivity <- map(seq(0, 1, 0.1), function(perc) {
    milestone_network <- milestone_network %>%
      left_join(alldistances, by=c("from", "to")) %>%
      group_by(from, to) %>%
      summarise(connectivity = quantile(distance, perc)) %>%
      ungroup() %>%
      pull(connectivity) %>%
      distance_ecdf(.)
  }) %>% unlist() %>% mean
  connectivity <- 1 - connectivity

  lst(connectivity)
}
