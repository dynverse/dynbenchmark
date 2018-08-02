approx_unique <- function(x, tolerance = .Machine$double.eps ^ 0.5) {
  abs(first(sort(x)) - last(sort(x))) < tolerance
}

approx_equal <- function(x, y, tolerance = .Machine$double.eps ^ 0.5) {
  abs(x - y) < tolerance
}

is_monotonic <- function(x, y) {
  all(x == cummax(x))
}

equal_identity <- lst(
  id = "equal_identity",
  description = "The score should always be the same when comparing the gold standard to itself",
  crossing = crossing(
    dataset_id = dataset_design %>% filter(topology_id == "bifurcation") %>% pull(dataset_id), ## TODO: add all datasets here
    method_id = "identity"
  ),
  assessment = function(scores) {
    lst(
      assessment = scores %>%
        group_by(metric_id) %>%
        summarise(check = approx_unique(score))
      ,
      plot_scores = scores %>%
        ggplot(aes(metric_id, score)) +
        geom_boxplot()
    )
  }
)

merge_bifurcation <- lst(
  id = "merge_bifurcation",
  description = "Merging the two branches after a bifurcation should lower the score",
  crossing = crossing(
    dataset_id = dataset_design %>% filter(topology_id == "bifurcation") %>% pull(dataset_id),
    method_id = c("identity", "merge_bifurcation")
  ),
  assessment = function(scores) {
    approx_unique <- function(x, tolerance = .Machine$double.eps ^ 0.5) {
      abs(first(sort(x)) - last(sort(x))) < tolerance
    }

    lst(
      assessment = scores %>%
        group_by(metric_id) %>%
        summarise(check = approx_unique(score))
      ,
      plot_scores = scores %>%
        ggplot(aes(metric_id, score, color = method_id)) +
        ggbeeswarm::geom_quasirandom()
    )
  }
)



positional_change <- lst(
  id = "positional_change",
  description = "Changing the positions of cells should lower the score smoothly",
  parameters = list(switch_cells = tibble(switch_perc = seq(0, 1, 0.1)) %>% mutate(id = as.character(switch_perc*10))),
  crossing = crossing(
    dataset_id = dataset_design %>% filter(topology_id == "bifurcation") %>% pull(dataset_id),
    method_id = "switch_cells",
    param_id = parameters$switch_cells$id
  ),
  assessment = function(scores) {
    scores <- left_join(scores, params$switch_cells, c("param_id" = "id"))

    assessment = scores %>%
      group_by(metric_id, switch_perc) %>%
      summarise(mean_score = mean(score)) %>%
      summarise(check = is_monotonic(switch_perc, mean_score))

    plot_scores = scores %>%
      ggplot(aes(switch_perc, score, colour = metric_id)) +
      geom_boxplot(aes(group = switch_perc)) +
      geom_smooth() +
      facet_wrap(~metric_id, scales = "free_y")

    lst(assessment, plot)
  }
)

# combine rules
rules <- list(
  equal_identity,
  merge_bifurcation,
  positional_change
) %>% map(~list(.) %>% list_as_tibble()) %>% bind_rows()

# check rules for contents
walkdf(rules, function(rule) {
  testthat::expect_true(all(
    c("id", "description", "crossing", "assessment") %in% names(rule)
  ))
})
