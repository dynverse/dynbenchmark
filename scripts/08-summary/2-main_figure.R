library(dynbenchmark)
library(tidyverse)
library(dynplot)

experiment("08-summary")

list2env(read_rds(result_file("results.rds")), environment())

# determine palettes
sc_col_fun <- function(palette) {
  function(x) {
    ifelse(is.na(x), "#444444", palette[cut(x, length(palette))])
  }
}
viridis_names <- c("viridis", "magma", "plasma", "inferno", "cividis")
scale_viridis_funs <- map(setNames(viridis_names, viridis_names), ~ sc_col_fun(viridisLite::viridis(101, option = .)))

topinf_colours <- topinf_types %>% select(name, colour) %>% deframe()
error_colours <- error_reasons %>% select(name, colour) %>% deframe()
maxtraj_colours <- trajectory_types %>% select(id, colour) %>% deframe()

exp_palettes <- c(summary = "inferno", qc = "viridis", benchmark = "magma", scaling = "cividis")

# COMBINE PLOT DATA INTO ONE TABLE

wrapper_type_map <- c(branch_trajectory = "traj", linear_trajectory = "linear", cyclic_trajectory = "cyclic", trajectory = "traj", cell_graph = "cell gr", cluster_graph = "clus gr", control = "control", dimred_projection = "dimred", end_state_probabilities = "prob")
data <-
  bind_rows(
    results %>%
      group_by(experiment, metric, category) %>%
      mutate(value = value / max(value)) %>%
      ungroup() %>%
      group_by(experiment) %>%
      mutate(colour = scale_viridis_funs[[exp_palettes[[experiment[[1]]]]]](value)) %>% # need to determine error reason colours
      ungroup() %>%
      mutate(
        metric = ifelse(category == "overall" & experiment != "summary", experiment, metric),
        experiment = ifelse(category == "overall" & experiment != "summary", "summary", experiment)
      ),
    method_info %>%
      transmute(
        method_id,
        name = method_name,
        topology_inference = ifelse(method_topology_inference == "parameter", "param", method_topology_inference),
        wrapper_type = wrapper_type_map[method_wrapper_type],
        most_complex = method_most_complex_trajectory_type
      ) %>%
      gather(metric, label, -method_id) %>%
      mutate(experiment = "method", category = "overall", colour = "black", placeholder = FALSE) # need to set colours
  )



# GENERATE METHOD POSITIONS
row_height <- 1
row_spacing <- .1

method_pos <-
  data_frame(
    method_id = factor(unique(results$method_id), levels = levels(results$method_id))
  ) %>%
  arrange(method_id) %>%
  mutate(
    method_i = row_number(),
    method_id = as.character(method_id),
    do_spacing = method_i != 1 & method_i %% 5 == 1,
    spacing = row_spacing,
    y = - (method_i * row_height + cumsum(spacing)),
    ymin = y - row_height / 2,
    ymax = y + row_height / 2
  )


# GENERATE METRIC POSITIONS
source(scripts_file("2a-metric_tbl.txt"))

metric_pos <-
  metric_pos %>%
  mutate(
    xsep = c(0, ifelse(experiment[-1] != experiment[-n()] | category[-1] != category[-n()], .5, .1)),
    xwidth = case_when(
      id == "name" ~ 6,
      id %in% c("wrty", "mcpx", "topi") ~ 2,
      geom == "bar" ~ 4,
      TRUE ~ 1
    ),
    show_label = ! name %in% c("Name"),
    xmax = cumsum(xwidth + xsep),
    xmin = xmax - xwidth,
    x = xmin + xwidth / 2
  )

# GENERATE HEADER POSITIONS
header_pos <-
  metric_pos %>%
  select(experiment, category, xmin, xmax) %>%
  mutate(row = row_number()) %>%
  gather(level, id, -row, -xmin, -xmax) %>%
  group_by(level, id) %>%
  summarise(xmin = min(xmin), xmax = max(xmax)) %>%
  ungroup() %>%
  arrange(level != "experiment", xmin) %>%
  mutate(
    y = ifelse(level == "experiment", 4.5, 3.5),
    key = ifelse(level == "experiment", letters[row_number()], ""),
    x = (xmin + xmax) / 2
  )


# PROCESS CIRCLES DATA
#' @examples
#' geom_types <- "trajtype"
#' met_row <- metric_pos %>% filter(geom == geom_types) %>% slice(1)
geom_data_processor <- function(geom_types, fun) {
  map_df(seq_len(nrow(metric_pos)), function(i) {
    if (!metric_pos$geom[[i]] %in% geom_types) {
      NULL
    } else {
      met_row <- metric_pos %>% slice(i)
      met <- crossing(met_row %>% select(-metric), data_frame(metric = unlist(met_row$metric)))
      dat <- data %>%
        filter(!placeholder) %>%
        inner_join(met, by = c("experiment", "category", "metric")) %>%
        left_join(method_pos, by = "method_id")
      fun(dat)
    }
  })
}
circle_data <- geom_data_processor("circle", function(dat) {
  dat %>% transmute(x0 = x, y0 = y, size = row_height / 2 * value, colour)
})
rect_data <- geom_data_processor("rect", function(dat) {
  dat %>% transmute(xmin, xmax, ymin, ymax, colour)
})
bar_data <- geom_data_processor("bar", function(dat) {
  dat %>% transmute(xmin = xmin, xmax = xmin + value * xwidth, ymin, ymax, colour)
})
invbar_data <- geom_data_processor("invbar", function(dat) {
  dat %>% transmute(xmin = xmax - value * xwidth, xmax, ymin, ymax, colour)
})
text_data <- geom_data_processor("text", function(dat) {
  dat %>% mutate(x = xmin, y, colour, label = ifelse(is.na(label), sprintf("%0.2f", value), label))
})
pie_data <- geom_data_processor("pie", function(dat) {
  dat %>%
    group_by(method_id) %>%
    transmute(
      y0 = y,
      x0 = x,
      pct = value / sum(value),
      rad = pct * 2 * pi,
      rad_end = cumsum(rad),
      rad_start = rad_end - rad,
      r0 = 0,
      r = row_height / 2,
      colour
    ) %>%
    filter(rad_end != rad_start) %>%
    ungroup()
})
barguides_data <- geom_data_processor(c("bar", "invbar"), function(dat) {
  data_frame(
    x = c(dat$xmin[[1]], dat$xmax[[1]]),
    y = min(dat$ymin),
    yend = max(dat$ymax)
  )
})
trajd <- geom_data_processor("trajtype", function(dat) {
  dat %>% transmute(xmin, xmax, ymin, ymax, topinf = label)
})

# CREATE LEGENDS
legy_start <- min(method_pos$ymin)

# leg_topinf <- topinf_types %>%
#   mutate(x = 1, y = seq_len(n())) %>%
#   rename(label = name, col = colour)
# leg_prior <- data_frame(
#   label = c("id", "#", "id", "#"),
#   type1 = c("optional_", "optional_", "required_", "required_"),
#   col = prior_type1cols[type1],
#   x = 1,
#   y = seq_along(type1),
#   explanation = c("optional cell/gene ids", "optional amount", "required cell/gene ids", "required amount")
# )
# leg_circles <- data_frame(
#   r = c(.05, .1, .2, .4, .6, .8, 1)/2,
#   x = cumsum(c(0, (.05 + r[-1] + r[-length(r)]))),
#   colbench = colbench_fun(r*2),
#   colqc = colqc_fun(r*2)
# )
#
# error_leg_df <- error_reasons %>%
#   rename(fill = colour) %>%
#   mutate(
#     rad_start = seq(0, pi, length.out = 5)[-5],
#     rad_end = seq(0, pi, length.out = 5)[-1],
#     rad = (rad_start + rad_end) / 2,
#     colour = rep("black", length(rad)),
#     lab_x = row_height * sin(rad),
#     lab_y = row_height * cos(rad),
#     hjust = rep(0, length(rad)),
#     vjust = seq(0, 1, length.out = length(rad)+2)[c(-1,-(length(rad)+2))]
#   )

# Stamp
stamp <- paste0("Generated on ", Sys.Date())

# MAKE PLOT
g1 <- ggplot() +

  # THEME SETTINGS
  coord_equal(expand = FALSE) +
  scale_alpha_identity() +
  scale_colour_identity() +
  scale_fill_identity() +
  cowplot::theme_nothing() +

  # SEPARATOR LINES
  geom_rect(aes(xmin = min(metric_pos$xmin), xmax = max(metric_pos$xmax), ymin = ymin - (spacing / 2), ymax = ymax + (spacing / 2)), method_pos %>% filter(method_i %% 2 == 1), fill = "#EEEEEE") +

  # METRIC AXIS
  geom_segment(aes(x = x, xend = x, y = -.3, yend = -.1), metric_pos %>% filter(show_label), size = .5) +
  geom_text(aes(x = x, y = 0, label = name), metric_pos %>% filter(show_label), angle = 30, vjust = 0, hjust = 0) +

  # GROUPING AXIS
  geom_segment(aes(x = xmin, xend = xmax, y = y, yend = y), header_pos, size = 1) +
  geom_text(aes(x = x, y = y+.5, label = id), header_pos, vjust = 1, hjust = 0.5, fontface = "bold") +
  geom_text(aes(x = xmin, y = y+.7, label = key), header_pos %>% filter(key != ""), vjust = 1, hjust = 0, fontface = "bold", size = 5) +

  # BAR GUIDES
  geom_segment(aes(x = x, xend = x, y = y, yend = yend), barguides_data, colour = "black", size = .5, linetype = "dashed") +
  # BARS
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = colour), bind_rows(bar_data, invbar_data), colour = "black", size = .25) +
  # RECTANGLES
  # geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = colour), rect_data, colour = "black", size = .25) +
  # geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), trajd, colour = "black", size = .25) +
  # CIRCLES
  ggforce::geom_circle(aes(x0 = x0, y0 = y0, fill = colour, r = size), circle_data, size = .25) +
  # STARS
  ggforce::geom_arc_bar(aes(x0 = x0, y0 = y0, r0 = r0, r = r, start = rad_start, end = rad_end, fill = colour), data = pie_data %>% filter(pct <= (1-1e-10)), size = .25) +
  ggforce::geom_circle(aes(x0 = x0, y0 = y0, r = r, fill = colour), data = pie_data %>% filter(pct > (1-1e-10)), size = .25) +
  # TEXT
  geom_text(aes(x = x, y = y, label = label, colour = colour), data = text_data, vjust = .5, hjust = 0) +

  # RESERVE SPACE
  expand_limits(x = c(max(metric_pos$xmax)+2), y = legy_start - 4.1) +

  # # LEGEND: TOPOLOGY
  # geom_text(aes(3, legy_start - 1, label = "Topology constraints"), data_frame(i = 1), hjust = 0, vjust = 0, fontface = "bold") +
  # geom_text(aes(3.5, legy_start - 1 - y * .7, label = label, colour = col), leg_topinf, hjust = 0, vjust = 0) +
  # geom_text(aes(6.5, legy_start - 1 - y * .7, label = explanation), leg_topinf, hjust = 0, vjust = 0) +
  #
  # # LEGEND: PRIOR
  # geom_text(aes(12.5, legy_start - 1, label = "Prior"), data_frame(i = 1), hjust = 0, vjust = 0, fontface = "bold") +
  # geom_text(aes(13, legy_start - 1 - y * .7, label = label, colour = col), leg_prior, hjust = 0, vjust = 0) +
  # geom_text(aes(14.5, legy_start - 1- y * .7, label = explanation), leg_prior, hjust = 0, vjust = 0) +
  #
  # # LEGEND: BENCHMARK
  # geom_text(aes(20.5, legy_start - 1, label = "Benchmark score"), data_frame(i = 1), hjust = 0, vjust = 0, fontface = "bold") +
  # ggforce::geom_circle(aes(x0 = 21.3 + x, y0 = legy_start - 2.3 + r, r = r, fill = colbench), size = .25, leg_circles) +
  # geom_text(aes(x = 21.3 + x, y = legy_start - 2.3 - .4, label = c("low", "high")), leg_circles %>% slice(c(1, n()))) +
  #
  # # LEGEND: PCT ERRORED
  # geom_text(aes(26, legy_start - 1, label = "Error reason"), data_frame(i = 1), hjust = 0, vjust = 0, fontface = "bold") +
  # ggforce::geom_arc_bar(aes(x0 = 26.5, y0 = legy_start - 2.5, r0 = 0, r = row_height*.75, start = rad_start, end = rad_end, fill = fill), size = .25, error_leg_df) +
  # ggforce::geom_arc_bar(aes(x0 = 26.5, y0 = legy_start - 2.5, r0 = 0, r = row_height*.75, start = rad_start, end = rad_end, fill = NA), size = .25, error_leg_df) +
  # geom_text(aes(x = 26.5 + lab_x + .5, y = legy_start - 2.5 + lab_y, label = label, vjust = vjust, hjust = hjust), error_leg_df) +
  # geom_segment(aes(x = 26.5, xend = 26.5, y = legy_start - 2.5, yend = legy_start - 2.5 + row_height*.75), data = data_frame(z = 1), size = .25) +
  #
  # # LEGEND: QC SCORE
  # geom_text(aes(34, legy_start - 1, label = "QC score"), data_frame(i = 1), hjust = 0, vjust = 0, fontface = "bold") +
  # ggforce::geom_circle(aes(x0 = 34.8 + x, y0 = legy_start - 2.3 + r, r = r, fill = colqc), size = .25, leg_circles) +
  # geom_text(aes(x = 34.8 + x, y = legy_start - 2.3 - .4, label = c("low", "high")), leg_circles %>% slice(c(1, n()))) +

  # GENERATION SENTENCE
  geom_text(aes(1, legy_start - 4, label = stamp), colour = "#cccccc", hjust = 0, vjust = 0) +
  expand_limits(y = legy_start - 4.3)

g1 <-
  plot_trajectory_types(plot = g1, trajectory_types = trajd$topinf, xmins = trajd$xmin, xmaxs = trajd$xmax, ymins = trajd$ymin, ymaxs = trajd$ymax, size = 1, geom = "circle", circ_size = .1)

# WRITE FILES
overview_fig_file <- result_file("overview.svg")
ggsave(overview_fig_file, g1, width = 20, height = 16)


