#' Create a figure containing predicted trajectories from the top methods on a couple of simple datasets

library(dynbenchmark)
library(tidyverse)

experiment("11-example_predictions")

# load dataset
designs <- list(
  list(
    id = "linear",
    dataset_id = "real/gold/developing-dendritic-cells_schlitzer",
    answers = dynguidelines::answer_questions(time = "5m", multiple_disconnected = FALSE, expect_topology = TRUE, expected_topology = "linear"),
    method_ids = c("scorpius", "monocle_ica", "slingshot", "paga", "waterfall", "tscan", "comp1")
  ),
  list(
    id = "bifurcating",
    dataset_id = "real/silver/fibroblast-reprogramming_treutlein",
    answers = dynguidelines::answer_questions(time = "5m", multiple_disconnected = FALSE, expect_topology = TRUE, expected_topology = "bifurcation"),
    method_ids = c("monocle_ddrtree", "slingshot", "paga", "pcreode", "scuba", "raceid_stemid", "dpt", "mst")
  ),
  list(
    id = "disconnected",
    # dataset_id = "real/placenta-trophoblast-differentiation-invasive_mca",
    dataset_id = "synthetic/dyntoy/disconnected_1",
    # dataset_id = "real/mouse-cell-atlas-combination-8",
    answers = dynguidelines::answer_questions(time = "1d", multiple_disconnected = TRUE, prior_information = "start_id", memory = "10GB"),
    method_ids = c("paga", "raceid_stemid", "mst")
  ),
  list(
    id = "cyclic",
    dataset_id = "synthetic/dyngen/72",
    # dataset_id = "synthetic/dyntoy/cyclic_1",
    # dataset_id = "real/cell-cycle_leng",
    answers = dynguidelines::answer_questions(time = "5m", multiple_disconnected = FALSE, expect_topology = TRUE, expected_topology = "cycle"),
    method_ids = c("angle", "raceid_stemid", "paga")
  )
)
# design <- designs[[2]]

# load in output models
to_load <- designs %>% map_df(~ data_frame(method_id = .$method_ids, dataset_id = .$dataset_id))
output <- benchmark_bind_results(
  load_models = TRUE,
  local_output_folder = derived_file("suite", experiment_id = "06-benchmark"),
  filter_fun = function(tib) tib %>% inner_join(to_load, by = c("dataset_id", "method_id"))
) %>%
  select(method_id, dataset_id, model, him)

plot_dimred_overviews <- list()

for (design in designs) {
  dataset <- load_dataset(design$dataset_id)

  dataset <- dataset %>% add_dimred(dyndimred::dimred_landmark_mds)
  # dataset <- dataset %>% add_dimred(dyndimred::dimred_umap)

  # plot reference dataset
  color_cells <- if (startsWith(dataset$source, "synthetic")) {"milestone"} else {"grouping"}
  milestones <- tibble(milestone_id = dataset$milestone_ids)
  plot_dimred_reference <- plot_dimred(
    dataset,
    color_cells,
    label_milestones = FALSE,
    dimred = get_dimred(dataset),
    milestones = milestones
  ) +
    labs(title = "Reference") +
    theme(plot.subtitle = element_text(hjust = 0.5), legend.position = "none")
  plot_dimred_reference

  # get methods
  # guidelines <- dynguidelines::guidelines(dataset, answers = design$answers)
  # method_ids <- guidelines$methods_aggr$method_id[1:3] %>% gsub("projected_gng", "gng", .) %>% discard(is.na)
  method_ids <- design$method_ids

  # get models of methods
  models <- output %>% filter(dataset_id == design$dataset_id) %>% slice(match(method_ids, method_id))
  models$model <- map(models$model, simplify_trajectory)

  # plot models
  dimred_plots <- models$model %>%
    map(plot_dimred, dimred = get_dimred(dataset), grouping = get_grouping(dataset), plot_milestone_network = TRUE) %>%
    map2(models$method_id, ~ . + ggtitle(label_method(.y)) + theme(legend.position = "none")) %>%
    patchwork::wrap_plots()

  dimred_plots

  # get consensus
  models$model <- map(models$model, dynwrap::add_cell_waypoints) %>% map(simplify_trajectory)
  models$model_ix <- seq_len(nrow(models))

  model_combinations <- crossing(model_ix1 = models$model_ix, model_ix2 = models$model_ix)
  model_combinations$correlation <- map2_dbl(
    models$model[model_combinations$model_ix1],
    models$model[model_combinations$model_ix2],
    function(model1, model2) {
      # make sure that the ce[ll ids match, TODO in dyneval: force this through a parameter
      model1$cell_ids <- dataset$cell_ids
      model2$cell_ids <- dataset$cell_ids
      dyneval::calculate_metrics(model1, model2, "correlation")$correlation
    }
  )
  model_combinations

  vote_mean <- function(model_combinations, metric) {
    metric <- rlang::enquo(metric)

    model_combinations %>%
      group_by(model_ix1) %>%
      filter(model_ix1 != model_ix2) %>%
      summarise(score = mean(!!metric)) %>%
      rename(model_ix = model_ix1)
  }

  vote_median <- function(model_combinations, metric) {
    metric <- rlang::enquo(metric)

    model_combinations %>%
      group_by(model_ix1) %>%
      filter(model_ix1 != model_ix2) %>%
      summarise(score = median(!!metric)) %>%
      rename(model_ix = model_ix1)
  }

  model_voting <- vote_median(model_combinations, correlation)

  ordered_models <- left_join(
    models,
    model_voting,
    "model_ix"
  ) %>%
    arrange(-score)

  # plot models
  plot_dimreds <- map(ordered_models$model, function(model) {
    plot_dimred(
      model,
      color_cells,
      grouping = get_grouping(dataset),
      milestone_percentages = dataset$milestone_percentages,
      milestones = milestones,
      dimred = get_dimred(dataset),
      plot_milestone_network = FALSE,
      alpha_cells = 0.5
    )
    # plot_dimred(model, dimred = get_dimred(dataset), plot_milestone_network = TRUE)
    # plot_dimred(model, "pseudotime", pseudotime = calculate_pseudotime(model %>% add_root(dataset$prior_information$start_id)), dimred = get_dimred(dataset), plot_milestone_network = TRUE)
    # plot_dimred(model, grouping = group_onto_trajectory_edges(model), color_cells = "grouping", dimred = get_dimred(dataset), plot_milestone_network = TRUE)
    # plot_dimred(model, grouping = group_onto_nearest_milestones(model), color_cells = "grouping", dimred = get_dimred(dataset), plot_milestone_network = TRUE)
    }) %>%
    map2(ordered_models$method_id, ~ . + ggtitle(label_method(.y))) %>%
    legend_at(theme_legend = guides(color = guide_legend(nrow = 1, ncol = length(unique(get_grouping(dataset))), title.theme = element_blank()))) %>%
    modify_at(1, ~ . + annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, fill = NA, color = "black") + labs(subtitle="(consensus)") + theme(plot.subtitle = element_text(hjust = 0.5)))

  plot_dimred_overview <- plot_dimreds %>%
    c(list(plot_dimred_reference), .) %>%
    patchwork::wrap_plots(nrow = 1)

  plot_dimred_overview

  plot_dimred_overviews[[design$id]] <- plot_dimred_overview
}


plot_dimred_overviews <- plot_dimred_overviews %>% map(patchwork::wrap_elements)

plot_example_predictions <- patchwork::wrap_plots(
  plot_dimred_overviews$linear + labs(tag = "a"),
  plot_dimred_overviews$bifurcating + labs(tag = "b"),
  patchwork::wrap_plots(
    plot_dimred_overviews$disconnected + labs(tag = "c"),
    plot_dimred_overviews$cyclic + labs(tag = "d"),
    ncol = 2
  ) %>% patchwork::wrap_elements(),
  nrow = 3
)

ggsave(result_file("example_predictions.pdf"), plot_example_predictions, width = 14, height = 8)
