merge_bifurcation <- list(
  id = "merge_bifurcation",
  description = "Merging the two branches after a bifurcation should lower the score substantially",
  crossing = crossing(
    dataset_id = dataset_design %>% filter(topology_id == "bifurcation") %>% pull(dataset_id),
    method_id = "merge_bifurcation"
  ),
  assessment = function(scores) {
    approx_unique <- function(x, tolerance = .Machine$double.eps ^ 0.5) {
      abs(first(sort(x)) - last(sort(x))) < tolerance
    }

    lst(
      assessment = scores %>%
        group_by(metric_id) %>%
        summarise(check = approx_unique(score)) %>%
        mutate(rule_id = "equal_gold_standard_score")
      ,
      plot = scores %>%
        ggplot(aes(metric_id, score)) +
        ggbeeswarm::geom_quasirandom()
    )
  }
)
