#' Score milestone aggregation
#' @param counts matrix of counts
#' @param cell_grouping data frame contain cell groups
#' @export
score_aggregation <- function(counts, cell_grouping) {
  cell_grouping <- cell_grouping %>% slice(match(rownames(counts), cell_id))

  ncount <- tibble(ncount=counts %>% rowMeans()) %>% bind_cols(cell_grouping)
  group_sds <- ncount %>% group_by(group_id) %>%
    summarise(var(ncount)) %>% pull(2)
  overall_sd <- ncount %>% pull(ncount) %>% var

  ncount <- tibble(ncount=counts %>% rowMeans()) %>% bind_cols(cell_grouping)
  ncount <- bind_rows(ncount, ncount %>% mutate(group_id="__all"))

  aggregation_plot <- ncount %>% ggplot() + geom_boxplot(aes(group_id, ncount, color=group_id))

  list(
    aggregation_sd_frac = mean(group_sds)/overall_sd,
    aggregation_plot = list(aggregation_plot)
  )
}
