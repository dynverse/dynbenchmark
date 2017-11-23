#' Compare the known transitions of the milestone_network with all other possible transitions
#'
#' @param counts_grouped grouped counts matrix
#' @param milestone_ids vector of all milestones
#' @param milestone_network data frame contain the milestone network
#' @export
score_milestone_transitions <- function(counts_grouped, milestone_ids, milestone_network) {
  if (!all(milestone_ids %in% rownames(counts_grouped))) stop("Counts should be grouped according to the milestones")

  milestone_combinations <- combn(milestone_ids, 2) %>% t %>% as_tibble() %>% rename(from=V1, to=V2)
  milestone_combinations$diff <- map2_dbl(as.character(milestone_combinations$from), as.character(milestone_combinations$to), ~sum((counts_grouped[.x,] - counts_grouped[.y,])^2))

  milestone_network_undirected <- bind_rows(
    milestone_network,
    milestone_network %>% rename(from=to, to=from)
  ) %>% mutate(in_network=TRUE)

  milestone_combinations <- milestone_combinations %>%
    left_join(milestone_network_undirected, by=c("from", "to")) %>%
    mutate(in_network=if_else(is.na(in_network), FALSE, TRUE))

  # make sure enough not-connected edges are present
  if (sum(!milestone_combinations$in_network) > 2) {
    test_result <- wilcox.test(milestone_combinations$diff[milestone_combinations$in_network], milestone_combinations$diff[!milestone_combinations$in_network], alternative="less")
  } else {
    test_result <- list(p.value=NA, statistic=NA)
  }

  transition_plot <- ggplot(milestone_combinations) + geom_boxplot(aes(in_network, diff))

  transition_frac <- mean(milestone_combinations$diff[!milestone_combinations$in_network]) / mean(milestone_combinations$diff[milestone_combinations$in_network])

  lst(
    transition_pval = -log10(test_result$p.value),
    transition_statistic=test_result$statistic,
    transition_plot=list(transition_plot),
    transition_frac,
    transition = list(milestone_combinations)
  )
}
