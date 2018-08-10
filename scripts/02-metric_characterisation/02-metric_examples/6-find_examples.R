library(tidyverse)
library(dynbenchmark)
library(dynplot)
library(PRISM)

experiment("02-metric_characterisation/6-find_examples")

cells_per_group <- 5
num_groups <- 3
spread <- 1 / num_groups / 5

cell_ids <- paste0("c", seq_len(cells_per_group * num_groups))

# pseudotime <- seq(0, 1, length.out = length(cell_ids) / cells_per_group) %>%
#   map(~ seq(-spread/2, spread/2, length.out = cells_per_group) + .) %>%
#   unlist() %>%
#   set_names(cell_ids) %>%
#   dynutils::scale_minmax()

milestone_ids <- paste0("M", seq_len(num_groups + 1))
milestone_network <- data_frame(
  from = milestone_ids %>% head(-1),
  to = milestone_ids %>% tail(-1),
  length = 1,
  directed = F
)
progressions <- map_df(seq_len(num_groups), function(groupi) {
  data_frame(
    cell_id = paste0("c", seq((groupi-1) * cells_per_group + 1, (groupi) * cells_per_group)),
    from = milestone_ids[[groupi]],
    to = milestone_ids[[groupi+1]],
    percentage = seq(0, .25, length.out = cells_per_group)
  )
})

dataset <- wrap_data(
  id = "example",
  cell_ids = cell_ids
) %>% add_trajectory(
  milestone_ids = milestone_ids,
  milestone_network = milestone_network,
  progressions = progressions,
  divergence_regions = NULL
) %>% add_cell_waypoints()

plot_default(dataset)

# set up possible networks
networks <- list(
  linear = list(
    milestone_ids = c("A", "B"),
    milestone_network = data_frame(from = "A", to = "B", length = 1, directed = FALSE),
    divergence_regions = NULL
  ),
  # bifurcation = list(
  #   milestone_ids = c("A", "B", "C", "D"),
  #   milestone_network = data_frame(from = c("A", "A", "A"), to = c("B", "C", "D"), length = 1, directed = FALSE),
  #   divergence_regions = data_frame(divergence_id = "div", milestone_id = c("A", "B", "C", "D"), is_start = milestone_id == "A")
  # )
  # bifurcation = list(
  #   milestone_ids = c("A", "B", "C", "D"),
  #   milestone_network = data_frame(from = c("A", "B", "B"), to = c("B" ,"C", "D"), length = 1, directed = FALSE),
  #   divergence_regions = data_frame(divergence_id = "div", milestone_id = c("A", "B", "C", "D"), is_start = milestone_id == "B")
  # )
  bifurcation = list(
    milestone_ids = c("A", "B", "C", "D"),
    milestone_network = data_frame(from = c("A", "A", "A"), to = c("B", "C", "D"), length = 1, directed = FALSE),
    divergence_regions = NULL
  ),
  trifurcation = list(
    milestone_ids = c("A", "B", "C", "D", "E"),
    milestone_network = data_frame(from = c("A", "A", "A", "A"), to = c("B", "C", "D", "E"), length = 1, directed = FALSE),
    divergence_regions = NULL
  ),
  cycle = list(
    milestone_ids = c("A", "B", "C"),
    milestone_network = data_frame(from = c("A", "B", "C"), to = c("B", "C", "A"), length = 1, directed = FALSE),
    divergence_regions = data_frame(divergence_id = "div", milestone_id = c("A", "B", "C"), is_start = milestone_id == "A")
  ),
  split = list(
    milestone_ids = c("A", "B", "C"),
    milestone_network = data_frame(from = c("A", "A"), to = c("B", "C"), length = 1, directed = FALSE),
    divergence_regions = data_frame(divergence_id = "div", milestone_id = c("A", "B", "C"), is_start = milestone_id == "A")
  )
) %>%
  map(function(l) {
    l$num_pcts <- length(cell_ids) * length(l$milestone_ids)
    l
  })

run_fun <- function(percentages, network) {
  milestone_ids <- network$milestone_ids
  milestone_network <- network$milestone_network
  divergence_regions <- network$divergence_regions

  milestone_percentages <- crossing(
    cell_id = cell_ids,
    milestone_id = milestone_ids
  ) %>%
    mutate(percentage = percentages) %>%
    group_by(cell_id)

  if (is.null(divergence_regions)) {
    milestone_percentages <- milestone_percentages %>%
      filter(milestone_id == "A" | percentage == max(percentage[milestone_id != "A"]))
  }

  milestone_percentages <- milestone_percentages %>%
    mutate(percentage = percentage / sum(percentage)) %>%
    ungroup()

  # return output
  wrap_prediction_model(
    cell_ids = cell_ids
  ) %>% add_trajectory(
    milestone_ids = milestone_ids,
    milestone_network = milestone_network,
    milestone_percentages = milestone_percentages,
    divergence_regions = divergence_regions
  ) %>% add_timings(
    timings =
      add_timing_checkpoint(NULL, "method_afterpreproc") %>%
      add_timing_checkpoint("method_aftermethod") %>%
      add_timing_checkpoint("method_afterpostproc")
  ) %>% add_cell_waypoints()
}
calculate_metrics2 <- function(dataset, model) {
  calculate_metrics(dataset, model) %>% mutate(rf_mse_inv = 1 - rf_mse, rf_mse_w = 1 - 4 * rf_mse)
}

# try out
nl <- networks$cycle
model <- run_fun(runif(nl$num_pcts), nl)
summary <- calculate_metrics2(dataset, model)

plot_default(model)

improve_df <- crossing(
  nettype = c("bifurcation", "split", "linear"),
  correlation = c(F, T),
  rf_mse_inv = c(F, T)
) %>% mutate(
  id = glue::glue("c = {ifelse(correlation, 'T', 'F')},r = {ifelse(rf_mse_inv, 'T', 'F')},nt = {nettype}")
)

outputs <-
  qsub_lapply(
    X = seq_len(nrow(improve_df)),
    qsub_config = override_qsub_config(num_cores = 24, execute_before = "source /scratch/irc/shared/dynverse/module_load_R.sh", r_module = NULL, max_wall_time = NULL),
    qsub_packages = c("tidyverse", "GA", "dynbenchmark"),
    FUN = function(i) {
      improve <- improve_df %>% extract_row_to_list(i)

      nl <- networks[[improve$nettype]]

      ga_fit <- GA::ga(
        type = "real-valued",
        min = rep(0, nl$num_pcts),
        max = rep(1, nl$num_pcts),
        maxiter = 100,
        popSize = 200,
        parallel = T,
        fitness = function(x) {

          model <- run_fun(x, nl)
          summary <- calculate_metrics2(dataset, model)

          metrics <- c(correlation = summary$correlation, rf_mse_inv = summary$rf_mse_w)

          if (!improve$correlation) {
            metrics[["correlation"]] <- 1 - metrics[["correlation"]]
          } else {
            metrics[["correlation"]] <- metrics[["correlation"]] ^ 2
          }

          if (!improve$rf_mse_inv) {
            metrics[["rf_mse_inv"]] <- 1 - metrics[["rf_mse_inv"]]
          } else {
            metrics[["rf_mse_inv"]] <- metrics[["rf_mse_inv"]] ^ 2
          }

          sum(metrics)
        })

      model <- run_fun(ga_fit@solution[1,], nl)
      summary <- calculate_metrics2(dataset, model)
      summary$id <- improve$id

      lst(id = improve$id, ga_fit, model, summary)
    })

write_rds(outputs, derived_file("outputs.rds"))

summary <- outputs %>% map_df(~.$summary)

cowplot::plot_grid(plotlist = unlist(map(outputs, function(o) {
  s <- o$summary
  o$model$id <- glue::glue("cor = {round(s$correlation, 2)}, rfmse = {round(s$rf_mse, 2)}, edge = {round(s$edge_flip, 2)}")
  list(plot_combined(dataset, o$model), plot_strip_connections(dataset, o$model))
}), recursive = F), ncol = 8) %>% ggsave(filename = figure_file("aspects.pdf"), width = 40, height = 20)

plot_default(dataset) %>% ggsave(filename = figure_file("dataset.pdf"), width = 5, height = 5)

#
# plot(ga_fit)
#
# plot_default(model)
# plot_combined(dataset, model)
# plot_strip_connections(dataset, model)
