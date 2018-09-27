library(dynbenchmark)
library(tidyverse)
library(paletteer)

experiment("02-metrics/03-aggregation")

create_discrete_palette <- function(values, ..., palette = NULL) {
  if (is.null(palette)) {
    palette <- paletteer_dynamic(..., n = length(values))
  }

  names(palette) <- values

  function(x) {
    palette[x]
  }
}

create_continuous_palette <- function(...) {
  palette <- paletteer_dynamic(..., n = 20)
  function(x) {
    palette[floor(as.numeric(x) * 19) + 1]
  }

}

table_original <- read_csv(raw_file("example.csv")) %>% mutate_if(is.character, forcats::fct_inorder)

palettes <- lst(
  dataset_id = unique(table_original$dataset_id) %>% create_discrete_palette("cartography", "blue.pal"),
  method_id = unique(table_original$method_id) %>% create_discrete_palette("cartography", "red.pal"),
  dataset_source = unique(table_original$dataset_source) %>% create_discrete_palette("cartography", "sand.pal"),
  trajectory_type = unique(table_original$trajectory_type) %>% create_discrete_palette("cartography", "green.pal"),
  metric_X = create_continuous_palette("cartography", "taupe.pal"),
  metric_Y = metric_X,
  metric_X_normalised = metric_X,
  metric_Y_normalised = metric_X,
  overall_score = metric_X
)

labellers <- lst(
  metric_X = function(x) sprintf("%0.2f", as.numeric(x)),
  metric_Y = metric_X,
  metric_X_normalised = metric_X,
  metric_Y_normalised = metric_X,
  overall_score = metric_X
)

get_color <- function(column, value) {
  map2_chr(column, value, function(column, value) {
    if(column %in% names(palettes)) {
      palettes[[column]](value)
    } else {
      "grey"
    }
  })
}

jump_on_change <- function(x) seq_along(x) + cumsum(lag(x, default = x[[1]]) != x)

normalise <- dynbenchmark:::.benchmark_aggregate_normalisation$normal
average <- function(x) mean(x)

tables <- lst(
  for_each_dataset = table_original %>%
    mutate(row_ix = jump_on_change(dataset_id)),

  normalised = for_each_dataset %>%
    group_by(dataset_id) %>%
    mutate_at(c("metric_X", "metric_Y"), funs(normalised = normalise)) %>%
    select(-metric_X, -metric_Y) %>%
    ungroup() %>%
    mutate(row_ix = jump_on_change(dataset_id)),

  `for_each_method,_trajectory_type_and_dataset_source` = normalised %>%
    mutate(row_ix = jump_on_change(paste0(trajectory_type, dataset_source))) %>%
    group_by(trajectory_type, dataset_source, method_id),

  aggregated_across_datasets = `for_each_method,_trajectory_type_and_dataset_source` %>%
    summarise_at(c("metric_X_normalised", "metric_Y_normalised"), average) %>%
    ungroup() %>%
    mutate(row_ix = row_number()),

  for_each_method_and_trajectory_type = aggregated_across_datasets %>%
    mutate(row_ix = jump_on_change(paste0(trajectory_type))) %>%
    group_by(trajectory_type, method_id),

  aggregated_across_dataset_sources = for_each_method_and_trajectory_type %>%
    summarise_at(c("metric_X_normalised", "metric_Y_normalised"), average) %>%
    ungroup() %>%
    mutate(row_ix = row_number()),

  for_each_method = aggregated_across_dataset_sources %>%
    mutate(row_ix = row_number()),

  aggregated_across_trajectory_types = for_each_method %>%
    group_by(method_id) %>%
    summarise_at(c("metric_X_normalised", "metric_Y_normalised"), average) %>%
    ungroup() %>%
    mutate(row_ix = row_number()),

  specific_scores = aggregated_across_trajectory_types,

  overall_score = specific_scores %>%
    group_by(method_id) %>%
    mutate(overall_score = dyneval::calculate_geometric_mean(metric_X_normalised, metric_Y_normalised)) %>%
    ungroup() %>%
    mutate(row_ix = row_number())
)


table <- tables$split_datasets

plot_table <- function(table) {
  table <- table %>%
    ungroup() %>%
    mutate(row_group = c(0, cumsum(diff(row_ix) != 1)))

  # group columns
  running_group <- function(x) {rep(seq_along(rle(x)$lengths), times = rle(x)$lengths)}
  table_gathered <- table %>%
    gather("column", "value", -row_ix, -row_group) %>%
    mutate(
      fill = get_color(column, value),
      column = fct_inorder(column),
      group = running_group(paste0(row_group, value)),
      label = map2_chr(value, column, ~if(as.character(.y) %in% names(labellers)) {labellers[[as.character(.y)]](.x)} else {.x})
    )

  # generate labels data
  labels <- table_gathered %>%
    group_by(label, column, group, fill) %>%
    summarise(row_ix = mean(row_ix))

  # expand the limits
  table_height <- (diff(range(table_gathered$row_ix)) + 1)
  table_width <- length(unique(table_gathered$column))

  table_gathered %>%
    ggplot(aes(column, row_ix)) +
    geom_tile(aes(fill = fill)) +
    geom_text(aes(label = label, color = shades::brightness(fill) > 0.5), data = labels) +
    scale_fill_identity() +
    scale_color_manual(values = c(`TRUE` = "black", `FALSE` = "white"), guide = "none") +
    scale_y_reverse("", breaks = NULL, expand = c(0, 0)) +
    scale_x_discrete("", position = "top", labels = function(x) label_short(x, width = 5), expand = c(0, 0)) +
    theme_pub()
}


plot_tables <- map2(tables, names(tables), function(table, table_id) {
  plot_table(table) + ggtitle(label_short(table_id, 30))
})



plot_arrow <- function(label = "") {
  ggplot() +
    geom_segment(aes(x = 0, y = 0, xend = 1, yend = 0), color = "black", arrow = arrow(type = "closed")) +
    geom_text(aes(x = 0.5, y = 0.2), label = label_short(label), vjust = 0) +
    scale_y_continuous(limits = c(-1, 1)) +
    theme_void()
}




# normalisation

patchwork::wrap_plots(
  plot_tables$for_each_dataset,
  plot_arrow("Normalise"),
  plot_tables$normalise,
  widths = c(1, 0.25, 1)
) %>% write_rds(result_file("normalisation_example.rds"))


{
  patchwork::wrap_plots(
    patchwork::wrap_plots(
      plot_tables$`for_each_method,_trajectory_type_and_dataset_source`,
      plot_arrow("Arithmetic mean"),
      plot_tables$aggregated_across_datasets,
      widths = c(1, 0.25, 1)
    ) %>% patchwork::wrap_elements(),
    patchwork::wrap_plots(
      plot_tables$for_each_method_and_trajectory_type,
      plot_arrow("Weighted arithmetic mean"),
      plot_tables$aggregated_across_dataset_sources,
      widths = c(1, 0.25, 1)
    ) %>% patchwork::wrap_elements(),
    patchwork::wrap_plots(
      plot_tables$for_each_method,
      plot_arrow("Arithmetic mean"),
      plot_tables$aggregated_across_trajectory_types,
      widths = c(1, 0.25, 1)
    ) %>% patchwork::wrap_elements(),

    ncol = 1
  ) +
    patchwork::plot_annotation(tag_levels = "a")
} %>% write_rds(result_file("aggregation_example.rds"))



patchwork::wrap_plots(
  plot_tables$specific_scores,
  plot_arrow("Geometric mean"),
  plot_tables$overall_score,
  widths = c(1, 0.25, 1)
) %>% write_rds(result_file("averaging_example.rds"))





#
#
#
# pdf("test.pdf")
# plot_tables
# dev.off()

# patchwork::wrap_plots(plot_tables, ncol = 2, byrow = TRUE)

