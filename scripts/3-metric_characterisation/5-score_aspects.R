library(dyneval)
library(dynplot)
library(tidyverse)
library(dynbenchmark)
library(dynutils)

experiment("3-metric_characterisation/5-score_aspects")

wrap <- function(x) {
  wrap_prediction_model(
    cell_ids = sort(unique(x$progressions$cell_id))
  ) %>% add_trajectory(
    milestone_ids = unique(c(x$milestone_network$from, x$milestone_network$to)),
    milestone_network = x$milestone_network,
    progressions = x$progressions,
    divergence_regions = NULL
  )
}

gs <- list(
  milestone_network = dyntoy:::generate_milestone_network("simple_linear"),
  progressions = tribble(
    ~cell_id, ~from, ~to, ~percentage,
    "1", "M1", "M2", 0.00,
    "2", "M1", "M2", 0.05,
    "3", "M1", "M2", 0.10,
    "4", "M1", "M2", 0.45,
    "5", "M1", "M2", 0.50,
    "6", "M1", "M2", 0.55,
    "7", "M1", "M2", 0.9,
    "8", "M1", "M2", 0.95,
    "9", "M1", "M2", 1.00
  )
) %>% wrap()

toys <- tribble(
  ~id, ~gs, ~toy,

  "gs", gs, gs,

  "bad_ordering",
  gs,
  list(
    milestone_network = gs$milestone_network,
    progressions = tribble(
      ~cell_id, ~from, ~to, ~percentage,
      "1", "M1", "M2", 0.55,
      "2", "M1", "M2", 0.45,
      "3", "M1", "M2", 0.50,
      "4", "M1", "M2", 0.05,
      "5", "M1", "M2", 0.10,
      "6", "M1", "M2", 0.00,
      "7", "M1", "M2", 1.00,
      "8", "M1", "M2", 0.90,
      "9", "M1", "M2", 0.95
    )
  ) %>% wrap(),
#
#   "bad_neighbourhood",
#   gs,
#   list(
#     milestone_network = gs$milestone_network,
#     progressions = tribble(
#       ~cell_id, ~from, ~to, ~percentage,
#       "1", "M1", "M2", 0.00,
#       "2", "M1", "M2", 0.15,
#       "3", "M1", "M2", 0.30,
#       "4", "M1", "M2", 0.30,
#       "5", "M1", "M2", 0.50,
#       "6", "M1", "M2", 0.65,
#       "7", "M1", "M2", 0.65,
#       "8", "M1", "M2", 0.85,
#       "9", "M1", "M2", 1.00
#     )
#   ) %>% wrap(),

  "bad_ordering_and_neighbourhood",
  gs,
  list(
    milestone_network = gs$milestone_network,
    progressions = gs$progressions %>% mutate(cell_id = set_names(sample(unique(cell_id)), unique(cell_id))[cell_id])
  ) %>% wrap(),

  "bad_topology",
  gs,
  list(
    milestone_network = dyntoy:::generate_milestone_network("consecutive_bifurcating") %>% mutate(length = ifelse(to %in% c("M5", "M6"), 0.1, 1)),
    progressions = tribble(
      ~cell_id, ~from, ~to, ~percentage,
      "1", "M1", "M2", 0.4,
      "2", "M1", "M2", 0.5,
      "3", "M1", "M2", 0.6,
      "4", "M2", "M3", 0.4,
      "5", "M2", "M3", 0.5,
      "6", "M2", "M3", 0.6,
      "7", "M3", "M4", 0.4,
      "8", "M3", "M4", 0.5,
      "9", "M3", "M4", 0.6
    )
  ) %>% wrap()
)

toys <- toys %>% add_row(
  id = "bad_topology,_ordering,_and_neighbourhood",
  gs = list(gs),
  toy = list(list(
    milestone_network = toys %>% filter(id == "bad_topology") %>% pull(toy) %>% first() %>% .$milestone_network,
    progressions = toys %>% filter(id == "bad_topology") %>% pull(toy) %>% first() %>% .$progressions %>% mutate(cell_id = set_names(sample(unique(cell_id)), unique(cell_id))[cell_id])
    ) %>% wrap())
)

dynutils::extract_row_to_list(toys, 1) %>% list2env(.GlobalEnv)
metrics <- c("correlation", "rf_mse", "edge_flip")
compare_toy <- function(gs, toy, metrics, id = "") {
  scores <- dyneval:::calculate_metrics(gs, toy, metrics = metrics)$summary
  scores %>% mutate(id = !!id)
}

scores <- toys %>% as.list() %>% pmap(compare_toy, metrics = metrics) %>% bind_rows()


##  ............................................................................
##  Plot scores heatmap                                                     ####

scores_heatmap <- scores[,c(metrics, "id")] %>%
  mutate(rf_mse = (1-rf_mse)) %>%
  mutate(id = factor(id, levels = rev(id))) %>%
  gather(metric, score, -id) %>%
  mutate(metric = factor(metric, levels = metrics)) %>%
  group_by(metric) %>%
  mutate(score_norm = (score - min(score))/(max(score) - min(score))) %>%
  ungroup() %>%
  ggplot() +
    geom_tile(aes(metric, id, fill = score_norm)) +
    #geom_text(aes(metric, id, label = round(score, 2))) +
    scale_fill_distiller("", breaks = c(0, 1), labels = c("Lowest score", "Best score"), direction = 1) +
    scale_x_discrete("Metric", expand = c(0, 0), labels = label_long) +
    scale_y_discrete("", expand = c(0, 0), labels = function(x) label_short(x, 20)) +
    cowplot::theme_cowplot() +
    coord_equal() +
    theme(
      legend.position = "top",
      legend.key.width = unit(30, "points"),
      axis.text.x = element_text(angle = 30, hjust = 1)
    )
scores_heatmap





##  ............................................................................
##  Plot connections                                                        ####

cell_colors <- shades::gradient(RColorBrewer::brewer.pal(3, "YlGnBu"), 101)[gs$progressions$percentage*100 + 1] %>% as.character() %>% set_names(gs$cell_ids)

tasks <- set_names(toys$toy, toys$id)
connections <- map(tasks, function(task) {
  dynplot::plot_connections(task$milestone_network, cell_progressions = task$progressions, cell_colors = cell_colors, orientation = -1)
}) %>%
  map2(., names(tasks), ~.+ggtitle(label_long(.y)) + theme(plot.title = element_text(hjust = 0.5))) %>%
  cowplot::plot_grid(plotlist = ., ncol = 1, labels = "auto", rel_heights = c(2,2,2,4,4))
connections

##  ............................................................................
##  Combine plots                                                           ####

score_aspects_plot <- cowplot::plot_grid(connections, scores_heatmap, rel_widths = c(0.6, 0.2))
score_aspects_plot
ggsave(figure_file("score_aspects.svg"), score_aspects_plot, width = 15, height = 8)
