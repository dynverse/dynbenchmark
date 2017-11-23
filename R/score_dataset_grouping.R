#' Score milestone grouping
#' @param counts counts matrix
#' @param cell_grouping data frame contain cell groups
#'
#' @importFrom cluster silhouette
#' @importFrom SCORPIUS correlation_distance
#'
#' @export
score_milestone_grouping <- function(counts, cell_grouping) {
  if(nrow(counts) > 1000) {
    frac <- 1000/nrow(counts)
    Coi <- cell_grouping %>% group_by(group_id) %>% sample_frac(frac) %>% pull(cell_id)
    counts <- counts[Coi, ]
  }

  cell_grouping <- cell_grouping %>% slice(match(rownames(counts), cell_id))

  result <- cluster::silhouette(as.numeric(factor(cell_grouping$group_id)), dmatrix=SCORPIUS::correlation_distance(counts))

  list(grouping_asw = summary(result)$si.summary[["Mean"]])
}
