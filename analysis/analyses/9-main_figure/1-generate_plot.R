library(dynalysis)
library(tidyverse)
library(dynplot)

experiment("9-main_figure")

list2env(read_rds(result_file("aggregated_data.rds", "9-main_figure")), environment())

method_ord <- method_tib %>% arrange(desc(harm_mean)) %>% .$method_name

# determine palettes
sc_fun <- function(x) {
  x / max(x, na.rm = T)
}
sc_col_fun <- function(palette) {
  function(x) {
    sc <- sc_fun(x)
    ifelse(is.na(sc), "#444444", palette[cut(sc, length(palette))])
  }
}
brewer_names <- RColorBrewer::brewer.pal.info %>% rownames()
scale_brewer_funs <- map(setNames(brewer_names, brewer_names), ~ sc_col_fun(colorRampPalette(RColorBrewer::brewer.pal(9, .))(101)))
viridis_names <- c("viridis", "magma", "plasma", "inferno", "cividis")
scale_viridis_funs <- map(setNames(viridis_names, viridis_names), ~ sc_col_fun(viridisLite::viridis(101, option = .)))

error_colours <- setNames(RColorBrewer::brewer.pal(3, "Set3"), c("pct_error_inside", "pct_memory_exceeded", "pct_time_exceeded"))
prior_colours <- setNames(RColorBrewer::brewer.pal(5, "Set1"), c("grouping_assignment", "grouping_assignment;start_cells;n_end_states", "marker_feature_ids", "n_end_states", "start_cells"))
topinf_colours <- setNames(RColorBrewer::brewer.pal(9, "Reds")[c(4,7,9)], c("free", "parameter", "fixed"))
trafo_colours <- setNames(RColorBrewer::brewer.pal(9, "Blues")[c(4,7,9)], c("minimal", "moderate", "extensive"))
maxtraj_colours <- setNames(trajectory_types$color, trajectory_types$id)

# coord calculation
row_height <- .95

colbench <- scale_viridis_funs$magma
colqc <- scale_viridis_funs$viridis
# colrem <- scale_brewer_funs$Greys
coltime <- scale_viridis_funs$cividis

method_tib <- method_tib %>%
  left_join(minis %>% select(maximal_trajectory_types = trajectory_type, maxtraj_replace_id = replace_id), by = "maximal_trajectory_types") %>%
  left_join(minis %>% select(prior_mini_id, prior_replace_id = replace_id), by = "prior_mini_id") %>%
  mutate(
    method_name_f = factor(method_name, levels = method_ord)
  ) %>%
  arrange(method_name_f) %>%
  mutate(
    method_i = seq_len(n()),
    do_spacing = method_i != 1 & method_i %% 5 == 1,
    spacing = ifelse(do_spacing, .15, 0.05),
    method_y = -method_i - cumsum(spacing),
    method_ymin = method_y - row_height / 2,
    method_ymax = method_y + row_height / 2,
    scalability_nfeat = ifelse(complexity_inferred == "nothing", NA, ifelse(grepl("nfeat2", complexity_inferred), "2", ifelse(grepl("nfeat", complexity_inferred), "1", "0"))),
    scalability_ncell = ifelse(complexity_inferred == "nothing", NA, ifelse(grepl("ncell2", complexity_inferred), "2", ifelse(grepl("ncell", complexity_inferred), "1", "0"))),
    rank_time_method = percent_rank(-time_method),
    rank_mean_var = percent_rank(-mean_var),
    rank_sets_seeds = ifelse(sets_seeds, 1, 2),
    rank_citations = percent_rank(n_citations),
    trafo = str_replace(output_transformation, ":.*", ""),
    pct_error_inside = pct_errored - pct_memory_exceeded - pct_time_exceeded,
    topinf_lab = c("free" = "free", "fixed" = "algo", "parameter" = "param")[topology_inference_type],
    trafo_lab = c("minimal" = "mild", "moderate" = "fair", "extensive" = "major")[trafo]
  ) %>%
  mutate_at(
    c(
      "harm_mean", "rank_correlation", "rank_edge_flip", "rank_rf_mse",
      "source_real", "source_synthetic",
      "trajtype_bifurcation", "trajtype_convergence", "trajtype_directed_acyclic_graph", "trajtype_directed_cycle",
      "trajtype_directed_graph", "trajtype_directed_linear", "trajtype_multifurcation", "trajtype_rooted_tree"
    ),
    funs(sc = sc_fun, sc_col = colbench)
  ) %>%
  mutate_at(
    c("qc_score", "qc_availability", "qc_behaviour", "qc_code_assurance", "qc_code_quality", "qc_documentation", "qc_paper"),
    funs(sc = sc_fun, sc_col = colqc)
  ) %>%
  mutate_at(
    c("rank_time_method", "rank_time_method"),
    funs(sc = sc_fun, sc_col = coltime)
  ) %>%
  mutate(
    prior_colour = prior_colours[prior_str],
    topinf_colour = topinf_colours[topology_inference_type],
    trafo_colour = trafo_colours[trafo],
    maxtraj_colour = maxtraj_colours[maximal_trajectory_types]
  )

# AXIS INFO
axis <-
  tribble(
    ~id,       ~label,                 ~xsep, ~xwidth, ~show_label, ~geom,      ~col,                                      ~value,
    "name",    "Name",                 0.0,   5,       F,           "custom",   NA,                                        NA,

    "maxt",    "Max. trajectory type", 0.1,   2,       T,           "custom",   "maxtraj_colour",                          NA,
    "topo",    "Topology constraints", 0.1,   1.5,     T,           "text",     "topinf_colour",                           "topinf_lab",
    "traf",    "Transformation",       0.1,   1.5,     T,           "text",     "trafo_colour",                            "trafo_lab",
    "prio",    "Required prior",       0.1,   2,       T,           "custom",   "prior_colour",                            NA,

    "harm",    "Score",                0.5,   5,       T,           "bar",      "harm_mean_sc_col",                        "harm_mean_sc",

    "corr",    "Correlation",          0.5,   1,       T,           "circle",   "rank_correlation_sc_col",                 "rank_correlation_sc",
    "edge",    "Edge flip",            0.1,   1,       T,           "circle",   "rank_edge_flip_sc_col",                   "rank_edge_flip_sc",
    "rfms",    "RF MSE",               0.1,   1,       T,           "circle",   "rank_rf_mse_sc_col",                      "rank_rf_mse_sc",

    "real",    "Real",                 0.5,   1,       T,           "circle",   "source_real_sc_col",                      "source_real_sc",
    "synt",    "Synthetic",            0.1,   1,       T,           "circle",   "source_synthetic_sc_col",                 "source_synthetic_sc",

    "line",    "Linear",               0.5,   1,       T,           "circle",   "trajtype_directed_linear_sc_col",        "trajtype_directed_linear_sc",
    "bifu",    "Bifurcation",          0.1,   1,       T,           "circle",   "trajtype_bifurcation_sc_col",            "trajtype_bifurcation_sc",
    "conv",    "Convergence",          0.1,   1,       T,           "circle",   "trajtype_convergence_sc_col",            "trajtype_convergence_sc",
    "cycl",    "Cycle",                0.1,   1,       T,           "circle",   "trajtype_directed_cycle_sc_col",         "trajtype_directed_cycle_sc",
    "mult",    "Multifurcation",       0.1,   1,       T,           "circle",   "trajtype_multifurcation_sc_col",         "trajtype_multifurcation_sc",
    "root",    "Rooted tree",          0.1,   1,       T,           "circle",   "trajtype_rooted_tree_sc_col",            "trajtype_rooted_tree_sc",
    "dagg",    "DAG",                  0.1,   1,       T,           "circle",   "trajtype_directed_acyclic_graph_sc_col", "trajtype_directed_acyclic_graph_sc",
    "grap",    "Graph",                0.1,   1,       T,           "circle",   "trajtype_directed_graph_sc_col",         "trajtype_directed_graph_sc",

    "time",    "Average time",         0.5,   1,       T,           "rect",     "rank_time_method_sc_col",                "rank_time_method_sc",
    "erro",    "% Errored",            0.1,   1,       T,           "pie",      list(error_colours),                      list(c("pct_error_inside", "pct_memory_exceeded", "pct_time_exceeded")),

    "qcsc",    "Score",                0.5,   5,       T,           "bar",      "qc_score_sc_col",                        "qc_score_sc",
    "qcav",    "Availability",         0.1,   1,       T,           "circle",   "qc_availability_sc_col",                 "qc_availability_sc",
    "qcbe",    "Behaviour",            0.1,   1,       T,           "circle",   "qc_behaviour_sc_col",                    "qc_behaviour_sc",
    "qcca",    "Code assurance",       0.1,   1,       T,           "circle",   "qc_code_assurance_sc_col",               "qc_code_assurance_sc",
    "qccq",    "Code quality",         0.1,   1,       T,           "circle",   "qc_code_quality_sc_col",                 "qc_code_quality_sc",
    "qcdo",    "Documentation",        0.1,   1,       T,           "circle",   "qc_documentation_sc_col",                "qc_documentation_sc",
    "qcpa",    "Paper",                0.1,   1,       T,           "circle",   "qc_paper_sc_col",                        "qc_paper_sc"
  ) %>%
  mutate(
    xmax = cumsum(xwidth + xsep),
    xmin = xmax - xwidth,
    x = xmin + xwidth / 2
  )
axtr <- map(
  seq_len(nrow(axis)),
  ~dynutils::extract_row_to_list(axis, .)
) %>%
  setNames(axis$id)

# define grouping information
grouping <-
  tribble(
    ~label, ~y, ~xmin, ~xmax,
    "Method characterisation", 3, axtr$name$xmin, axtr$prio$xmax,
    "Benchmark pipeline", 3, axtr$harm$xmin, axtr$erro$xmax,
    "Per metric", 2, axtr$corr$xmin, axtr$rfms$xmax,
    "Per source", 2, axtr$real$xmin, axtr$synt$xmax,
    "Per trajectory type", 2, axtr$line$xmin, axtr$grap$xmax,
    "Execution", 2, axtr$time$xmin, axtr$erro$xmax,
    "Quality control", 3, axtr$qcsc$xmin, axtr$qcpa$xmax
  ) %>%
  mutate(
    x = (xmin + xmax) / 2
  )

# PROCESS CIRCLES DATA
circle_data <- map_df(seq_len(nrow(axis)), function(i) {
  if (axis$geom[[i]] != "circle") return(NULL)
  axis_l <- axis %>% dynutils::extract_row_to_list(i)
  method_tib %>%
    select_(y0 = "method_y", col = axis_l$col, value = axis_l$value) %>%
    mutate(
      size = .5 * value,
      x0 = axis_l$x
    )
})
rect_data <- map_df(seq_len(nrow(axis)), function(i) {
  if (axis$geom[[i]] != "rect") return(NULL)
  axis_l <- axis %>% dynutils::extract_row_to_list(i)
  method_tib %>%
    select_(ymin = "method_ymin", ymax = "method_ymax", col = axis_l$col) %>%
    mutate(
      xmin = axis_l$xmin,
      xmax = axis_l$xmax
    )
})
bar_data <- map_df(seq_len(nrow(axis)), function(i) {
  if (axis$geom[[i]] != "bar") return(NULL)
  axis_l <- axis %>% dynutils::extract_row_to_list(i)
  method_tib %>%
    select_(ymin = "method_ymin", ymax = "method_ymax", col = axis_l$col, value = axis_l$value) %>%
    mutate(
      xmin = axis_l$xmin,
      xmax = axis_l$xmin + value * axis_l$xwidth
    )
})
pie_data <- map_df(seq_len(nrow(axis)), function(i) {
  if (axis$geom[[i]] != "pie") return(NULL)
  axis_l <- axis %>% dynutils::extract_row_to_list(i)
  cols <- axis_l$col[[1]]
  values <- axis_l$value[[1]]
  method_tib %>%
    select(method_short_name, y0 = method_y, one_of(values)) %>%
    gather(piece, value, one_of(values)) %>%
    arrange(method_short_name, piece) %>%
    group_by(method_short_name) %>%
    mutate(
      col = cols[piece],
      x0 = axis_l$x,
      rad = value * 2 * pi,
      rad_end = cumsum(rad),
      rad_start = rad_end - rad,
      r0 = 0,
      r = .5
    )
})
text_data <- map_df(seq_len(nrow(axis)), function(i) {
  if (axis$geom[[i]] != "text") return(NULL)
  axis_l <- axis %>% dynutils::extract_row_to_list(i)
  method_tib %>%
    select_(y = "method_y", col = axis_l$col, label = axis_l$value) %>%
    mutate(
      x = axis_l$x
    )
})

# MAKE PLOT
g <- ggplot(method_tib) +

  # THEME SETTINGS
  coord_equal(expand = FALSE) +
  scale_alpha_identity() +
  scale_colour_identity() +
  scale_fill_identity() +
  cowplot::theme_nothing() +

  # METRIC AXIS
  geom_segment(aes(x = x, xend = x, y = -.3, yend = -.1), axis %>% filter(show_label), size = .5) +
  geom_text(aes(x = x, y = 0, label = label), axis %>% filter(show_label), angle = 30, vjust = 0, hjust = 0) +
  expand_limits(y = 2) +

  # GROUPING AXIS
  geom_segment(aes(x = xmin, xend = xmax, y = y, yend = y), grouping, size = 1) +
  geom_text(aes(x = x, y = y+.2, label = label), grouping, vjust = 0, hjust = 0.5, fontface = "bold") +
  expand_limits(y = 4) +

  # METHOD NAMES
  geom_text(aes(x = axtr$name$xmax, y = method_y, label = method_name), hjust = 1, vjust = .5) +

  # SEPARATOR LINES
  geom_segment(aes(x = axtr$name$xmin, xend = axtr$qcpa$xmax, y = method_ymax+(spacing/2), yend = method_ymax+(spacing*.65)), method_tib %>% filter(do_spacing), size = .25, linetype = "dashed", colour = "darkgray") +

  # BARS
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = col), bar_data, colour = "black", size = .25) +
  # RECTANGLES
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = col), rect_data, colour = "black", size = .25) +
  # CIRCLES
  ggforce::geom_circle(aes(x0 = x0, y0 = y0, fill = col, r = size), circle_data, size = .25) +
  # STARS
  ggforce::geom_arc_bar(aes(x0 = x0, y0 = y0, r0 = r0, r = r, start = rad_start, end = rad_end, fill = col), data = pie_data, size = .25) +
  ggforce::geom_arc_bar(aes(x0 = x0, y0 = y0, r0 = r0, r = r, start = rad_start, end = rad_end, fill = NA), data = pie_data, size = .25) +
  geom_segment(aes(x = x0, xend = x0, y = y0 + r0, yend = y0 + r), data = pie_data, size = .25) +
  # TEXT
  geom_text(aes(x = x, y = y, label = label, colour = col), data = text_data, vjust = .5, hjust = .5) +

  # TRAJ TYPES
  geom_rect(aes(xmin = axtr$maxt$xmin, xmax = axtr$maxt$xmax, ymin = method_ymin, ymax = method_ymax, alpha = maxtraj_replace_id, fill = "#ABCDEF"), colour = "black", size = .25) +

  # PRIORS
  geom_rect(aes(xmin = axtr$prio$xmin, xmax = axtr$prio$xmax, ymin = method_ymin, ymax = method_ymax, alpha = prior_replace_id, fill = "#ABCDEF"), colour = "black", size = .25) +

  # RESERVE SPACE
  expand_limits(x = c(56))

overview_fig_file <- figure_file("overview.svg")
ggsave(overview_fig_file, g, width = 16, height = 10)
xml2::read_xml(overview_fig_file) %>% replace_svg(minis) %>% xml2::write_xml(overview_fig_file)
