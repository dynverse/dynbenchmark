approx_unique <- function(x, tolerance = .Machine$double.eps ^ 0.5) {
  abs(first(sort(x)) - last(sort(x))) < tolerance
}

approx_equal <- function(x, y, tolerance = .Machine$double.eps ^ 0.5) {
  abs(x - y) < tolerance
}

is_monotonic <- function(x, y, decreasing = TRUE) {
  if (decreasing) {
    all(diff(y[order(x)]) < 0)
  } else {
    all(diff(y[order(x)]) > 0)
  }
}

equal_identity <- lst(
  id = "equal_identity",
  description = "The score should always be the same when comparing the gold standard to itself",
  crossing = crossing(
    dataset_id = dataset_design %>% filter(topology_id == "bifurcation") %>% pull(dataset_id), ## TODO: add all datasets here
    method_id = "identity"
  ),
  assessment = function(scores, rule) {
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


rule_lower <- function(
  id,
  description,
  dataset_ids,
  method_id
) {
  lst(
    id = id,
    description = description,
    crossing = crossing(
      dataset_id = dataset_ids,
      method_id = c("identity", method_id)
    ),
    assessment = function(scores, rule) {
      scores <- scores %>%
        mutate(method_id = factor(method_id, levels = c("identity", !!method_id)))

      print(scores$method_id)

      assessment <- scores %>%
        spread(method_id, score) %>%
        group_by(metric_id) %>%
        summarise(check = all(!!method_id < identity))

      plot_scores <- scores %>%
        ggplot(aes(method_id, score)) +
        geom_line(aes(group = dataset_id)) +
        geom_point() +
        facet_wrap(~metric_id, scales = "free_y")

      plot_datasets <- scores %>%
        group_by(method_id) %>%
        filter(row_number() == 1) %>%
        arrange(method_id) %>%
        pmap(function(model, method_id, ...) {plot_graph(model) + ggtitle(method_id)}) %>%
        patchwork::wrap_plots()

      lst(
        assessment,
        plot_scores,
        plot_datasets
      )
    }
  )
}

merge_bifurcation <- rule_lower(
  id = "merge_bifurcation",
  description = "Merging the two branches after a bifurcation should lower the score",
  dataset_ids = dataset_design %>% filter(topology_id == "bifurcation") %>% pull(dataset_id),
  method_id = "merge_bifurcation"
)

concatenate_bifurcation <- rule_lower(
  id = "concatenate_bifurcation",
  description = "Concatenating one branch of a bifurcation to the other branch should lower the score",
  dataset_ids = dataset_design %>% filter(topology_id == "bifurcation") %>% pull(dataset_id),
  method_id = "concatenate_bifurcation"
)





switch_cells <- lst(
  id = "switch_cells",
  description = "Switching the positions of cells should lower the score smoothly",
  parameters = list(switch_cells = tibble(switch_perc = seq(0, 1, 0.1)) %>% mutate(id = as.character(switch_perc*10))),
  crossing = crossing(
    dataset_id = dataset_design %>% filter(topology_id == "bifurcation") %>% pull(dataset_id),
    method_id = "switch_cells",
    param_id = parameters$switch_cells$id
  ),
  assessment = function(scores, rule) {
    scores <- left_join(scores, rule$parameters$switch_cells, c("param_id" = "id"))

    assessment = scores %>%
      group_by(metric_id, switch_perc) %>%
      summarise(mean_score = mean(score)) %>%
      summarise(check = is_monotonic(switch_perc, mean_score))

    plot_scores = scores %>%
      ggplot(aes(switch_perc, score, colour = metric_id)) +
      geom_boxplot(aes(group = switch_perc)) +
      geom_smooth() +
      facet_wrap(~metric_id, scales = "free_y")

    lst(assessment, plot_scores)
  }
)


switch_edges <- lst(
  id = "switch_edges",
  description = "Switching the edges of the topology should lower the score smoothly",
  parameters = list(switch_edges = tibble(switch_perc = seq(0, 1, 0.1)) %>% mutate(id = as.character(switch_perc*10))),
  crossing = crossing(
    dataset_id = dataset_design %>% filter(topology_id == "bifurcation") %>% pull(dataset_id),
    method_id = "switch_edges",
    param_id = parameters$switch_edges$id
  ),
  assessment = function(scores, rule) {
    scores <- left_join(scores, rule$parameters$switch_edges, c("param_id" = "id"))

    assessment = scores %>%
      group_by(metric_id, switch_perc) %>%
      summarise(mean_score = mean(score)) %>%
      summarise(check = is_monotonic(switch_perc, mean_score))

    plot_scores = scores %>%
      ggplot(aes(switch_perc, score, colour = metric_id)) +
      geom_boxplot(aes(group = switch_perc)) +
      geom_smooth() +
      facet_wrap(~metric_id, scales = "free_y")

    lst(assessment, plot_scores)
  }
)







# combine rules
rules <- list(
  # equal_identity,
  # merge_bifurcation,
  concatenate_bifurcation
  # switch_cells,
  # switch_edges
) %>% map(~list(.) %>% list_as_tibble()) %>% bind_rows()

# check rules for contents
walkdf(rules, function(rule) {
  testthat::expect_true(all(
    c("id", "description", "crossing", "assessment") %in% names(rule)
  ))
})
