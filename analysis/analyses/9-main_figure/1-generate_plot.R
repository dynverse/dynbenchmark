library(dynalysis)
library(tidyverse)
library(dynplot)

experiment("9-main_figure")

list2env(read_rds(result_file("aggregated_data.rds", "9-main_figure")), environment())

# determine palettes
sc_fun <- scale_minmax
sc_fun <- function(x) x / max(x, na.rm = TRUE)
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

error_colours <- setNames(RColorBrewer::brewer.pal(4, "Set3"), c("pct_memory_exceeded", "pct_time_exceeded", "pct_allerrored", "pct_stochastic"))
maxtraj_colours <- setNames(trajectory_types$color, trajectory_types$id)

coloverall <- "inferno"
colbench <- "magma"
colqc <- "viridis"
coltime <-  "cividis"
coloverall_fun <- scale_viridis_funs[[coloverall]]
colbench_fun <- scale_viridis_funs[[colbench]]
colqc_fun <- scale_viridis_funs[[colqc]]
coltime_fun <- scale_viridis_funs[[coltime]]

prior_type1cols <- c("required_" = "#cc2400", "optional_" = "#00aaed", "_" = "#00ab1b")
prior_type1_fun <- function(x) ifelse(is.na(x), "", gsub("_.*", "", x))
prior_type1col_fun <- function(x) prior_type1cols[paste0(prior_type1_fun(x), "_")]
prior_type2_fun <- function(x) ifelse(is.na(x), "", gsub(".*_", "", x))

# PROCESS COORDINATES AND COLOURS
row_height <- 1
row_spacing <- .1

method_tib <- method_tib %>%
  left_join(minis %>% select(maximal_trajectory_types = trajectory_type, maxtraj_replace_id = replace_id), by = "maximal_trajectory_types") %>%
  arrange(method_name_f) %>%
  mutate(
    method_i = seq_len(n()),
    do_spacing = method_i != 1 & method_i %% 5 == 1,
    spacing = row_spacing,
    method_y = - (method_i * row_height + cumsum(spacing)),
    method_ymin = method_y - row_height / 2,
    method_ymax = method_y + row_height / 2,
    rank_time_method = percent_rank(-time_method),
    trafo = str_replace(output_transformation, ":.*", ""),
    topinf_lab = c("free" = "free", "fixed" = "fixed", "parameter" = "param")[topology_inference_type],
    avg_time_lab = ifelse(time_method < 60, paste0(round(time_method), "s"), ifelse(time_method < 3600, paste0(round(time_method / 60), "m"), paste0(round(time_method / 3600), "h"))),
    remove_results = pct_errored > .49,
    pct_succeeded = 1 - pct_errored,
    pct_errored_lab = paste0(round(pct_errored * 100), "%"),
    missing_qc_reason = ifelse(!is_na_qc, "", ifelse(method_short_name %in% c("periodpc", "atan", "comp1", "random", "gng"), "Control", "Missing")),
    black = "black"
  ) %>%
  mutate_at(
    c(
      "overall_metric", "overall_source", "overall_trajtype", "overall_benchmark",
      "harm_mean", "norm_correlation", "norm_edge_flip", "norm_rf_mse",
      "source_real", "source_synthetic",
      "trajtype_bifurcation", "trajtype_convergence", "trajtype_directed_acyclic_graph", "trajtype_directed_cycle",
      "trajtype_directed_graph", "trajtype_directed_linear", "trajtype_multifurcation",
      # "trajtype_disconnected_directed_graph",
      "trajtype_rooted_tree"
    ),
    funs(sc = sc_fun, sc_col = colbench_fun)
  ) %>%
  mutate_at(
    c(
      "overall", "overall"
    ),
    funs(sc = sc_fun, sc_col = coloverall_fun)
  ) %>%
  mutate_at(
    c("prior_start", "prior_end", "prior_states", "prior_genes"),
    funs(type1 = prior_type1_fun, type2 = prior_type2_fun, type1col = prior_type1col_fun)
  ) %>%
  mutate_at(
    c(
      "overall_qc", "overall_qccat", "overall_qcapp",
      "qc_score", "qc_cat_availability", "qc_cat_behaviour", "qc_cat_code_assurance", "qc_cat_code_quality",
      "qc_cat_documentation", "qc_cat_paper", "qc_app_developer_friendly", "qc_app_good_science", "qc_app_user_friendly"),
    funs(sc = sc_fun, sc_col = colqc_fun)
  ) %>%
  mutate_at(
    c("rank_time_method", "pct_succeeded", "pct_errored"),
    funs(sc = sc_fun, sc_col = coltime_fun)
  ) %>%
  mutate(
    topinf_colour = topinf_colours[topology_inference_type],
    maxtraj_colour = maxtraj_colours[maximal_trajectory_types],
    avg_time_lab_colour = ifelse(rank_time_method_sc < .5, "white", "black"),
    pct_errored_lab_colour = ifelse(pct_errored_sc > .5, "white", "black")
  )

# AXIS INFO
axis <-
  tribble(
    ~id,       ~label,                 ~xsep, ~xwidth, ~show_label, ~geom,      ~filter_removed, ~col,                                      ~value,
    "name",    "Name",                 0.0,   6,       F,           "custom",   F,               NA,                                        NA,

    "maxt",    "Type",                 0.1,   2,       T,           "custom",   F,               "maxtraj_colour",                          NA,
    "topo",    "Topo. constr.",        0.1,   1,       T,           "text",     F,               "topinf_colour",                           "topinf_lab",
    "prst",    "Start",                0.1,   1,       T,           "text",     F,               "prior_start_type1col",                    "prior_start_type2",
    "pren",    "End",                  0.1,   1,       T,           "text",     F,               "prior_end_type1col",                      "prior_end_type2",
    "prgr",    "States",               0.1,   1,       T,           "text",     F,               "prior_states_type1col",                   "prior_states_type2",
    "prge",    "Genes",                0.1,   1,       T,           "text",     F,               "prior_genes_type1col",                    "prior_genes_type2",


    "harm",    "Benchmark",            1.0,   4,       T,           "invbar",   F,               "overall_benchmark_sc_col",                "overall_benchmark_sc",
    "qcsc",    "QC",                   0.0,   4,       T,           "bar",      F,               "overall_qc_sc_col",                       "overall_qc_sc",
    "qcre",     "",                     -4,   4,       F,           "text",     F,               "black",                                   "missing_qc_reason",

    "corr",    "Ordering",             1.0,   1,       T,           "circle",   F,               "norm_correlation_sc_col",                 "norm_correlation_sc",
    "rfms",    "Neighbourhood",        0.1,   1,       T,           "circle",   F,               "norm_rf_mse_sc_col",                      "norm_rf_mse_sc",
    "edge",    "Topology",             0.1,   1,       T,           "circle",   F,               "norm_edge_flip_sc_col",                   "norm_edge_flip_sc",

    "real",    "Real",                 0.5,   1,       T,           "circle",   F,               "source_real_sc_col",                      "source_real_sc",
    "synt",    "Synthetic",            0.1,   1,       T,           "circle",   F,               "source_synthetic_sc_col",                 "source_synthetic_sc",

    "line",    "Linear",               0.5,   1,       T,           "circle",   F,               "trajtype_directed_linear_sc_col",         "trajtype_directed_linear_sc",
    "bifu",    "Bifurcation",          0.1,   1,       T,           "circle",   F,               "trajtype_bifurcation_sc_col",             "trajtype_bifurcation_sc",
    "conv",    "Convergence",          0.1,   1,       T,           "circle",   F,               "trajtype_convergence_sc_col",             "trajtype_convergence_sc",
    "mult",    "Multifurcation",       0.1,   1,       T,           "circle",   F,               "trajtype_multifurcation_sc_col",          "trajtype_multifurcation_sc",
    "root",    "Rooted tree",          0.1,   1,       T,           "circle",   F,               "trajtype_rooted_tree_sc_col",             "trajtype_rooted_tree_sc",
    "dagg",    "DAG",                  0.1,   1,       T,           "circle",   F,               "trajtype_directed_acyclic_graph_sc_col",  "trajtype_directed_acyclic_graph_sc",
    "cycl",    "Cycle",                0.1,   1,       T,           "circle",   F,               "trajtype_directed_cycle_sc_col",          "trajtype_directed_cycle_sc",
    "grap",    "Cyclic graph",         0.1,   1,       T,           "circle",   F,               "trajtype_directed_graph_sc_col",          "trajtype_directed_graph_sc",
    # "digr",    "Disconnected Graph",   0.1,   1,       T,           "circle",   T,               "trajtype_disconnected_directed_graph_sc_col", "trajtype_disconnected_directed_graph_sc",

    "time",    "Average time",         0.5,   1,       T,           "rect",     F,               "rank_time_method_sc_col",                 "rank_time_method_sc",
    "timl",    "Average time label",    -1,   1,       F,           "text",     F,               "avg_time_lab_colour",                     "avg_time_lab",
    "erpc",    "% Errored",            0.1,   1,       T,           "rect",     F,               "pct_succeeded_sc_col",                    "pct_succeeded_sc",
    "erpl",    "% Errored label",       -1,   1,       F,           "text",     F,               "pct_errored_lab_colour",                  "pct_errored_lab",
    "erro",    "Error reason",         0.1,   1,       T,           "pie",      F,               list(error_colours),                       list(c("pct_memory_exceeded", "pct_time_exceeded", "pct_allerrored", "pct_stochastic")),

    "qcdf",    "Developer friendly",   1.0,   1,       T,           "circle",   F,               "qc_app_developer_friendly_sc_col",        "qc_app_developer_friendly_sc",
    "qcuf",    "User friendly",        0.1,   1,       T,           "circle",   F,               "qc_app_user_friendly_sc_col",             "qc_app_user_friendly_sc",
    "qcgs",    "Future-proof",         0.1,   1,       T,           "circle",   F,               "qc_app_good_science_sc_col",              "qc_app_good_science_sc",

    "qcav",    "Availability",         0.5,   1,       T,           "circle",   F,               "qc_cat_availability_sc_col",              "qc_cat_availability_sc",
    "qcbe",    "Behaviour",            0.1,   1,       T,           "circle",   F,               "qc_cat_behaviour_sc_col",                 "qc_cat_behaviour_sc",
    "qcca",    "Code assurance",       0.1,   1,       T,           "circle",   F,               "qc_cat_code_assurance_sc_col",            "qc_cat_code_assurance_sc",
    "qccq",    "Code quality",         0.1,   1,       T,           "circle",   F,               "qc_cat_code_quality_sc_col",              "qc_cat_code_quality_sc",
    "qcdo",    "Documentation",        0.1,   1,       T,           "circle",   F,               "qc_cat_documentation_sc_col",             "qc_cat_documentation_sc",
    "qcpa",    "Paper",                0.1,   1,       T,           "circle",   F,               "qc_cat_paper_sc_col",                     "qc_cat_paper_sc"
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
    ~label,                        ~y, ~xmin,          ~xmax,          ~key,
    "Method characterisation",     3,  axtr$name$xmin, axtr$prge$xmax, "a",
    "Priors",                      2,  axtr$prst$xmin, axtr$prge$xmax, "",
    "Overall score",               3,  axtr$harm$xmin, axtr$qcsc$xmax, "b",
    "Benchmark",                   3,  axtr$corr$xmin, axtr$erro$xmax, "c",
    "Per metric",                  2,  axtr$corr$xmin, axtr$edge$xmax, "",
    "Per source",                  2,  axtr$real$xmin, axtr$synt$xmax, "",
    "Per trajectory type",         2,  axtr$line$xmin, axtr$grap$xmax, "",
    # "Per trajectory type",         2,  axtr$line$xmin, axtr$digr$xmax, "",
    "Execution",                   2,  axtr$time$xmin, axtr$erro$xmax, "",
    "Quality control",             3,  axtr$qcdf$xmin, axtr$qcpa$xmax, "d",
    "Practicality",                2,  axtr$qcdf$xmin, axtr$qcgs$xmax, "",
    "Categories",                  2,  axtr$qcav$xmin, axtr$qcpa$xmax, ""
  ) %>%
  mutate(
    x = (xmin + xmax) / 2
  )

# PROCESS CIRCLES DATA
geom_data_processor <- function(geom_type, fun) {
  map_df(seq_len(nrow(axis)), function(i) {
    if (axis$geom[[i]] != geom_type) return(NULL)
    axis_l <- axis %>% dynutils::extract_row_to_list(i)
    z <- method_tib
    if (axis_l$filter_removed) {
      z <- z %>% filter(!remove_results)
    }
    fun(axis_l, z)
  })
}
circle_data <- geom_data_processor(
  "circle",
  function(axis_l, method_tib_filt) {
    method_tib_filt %>%
      select_(y0 = "method_y", col = axis_l$col, value = axis_l$value) %>%
      mutate(
        size = row_height / 2 * value,
        x0 = axis_l$x
      )
  })
rect_data <- geom_data_processor(
  "rect",
  function(axis_l, method_tib_filt) {
    method_tib_filt %>%
      select_(ymin = "method_ymin", ymax = "method_ymax", col = axis_l$col) %>%
      mutate(
        xmin = axis_l$xmin,
        xmax = axis_l$xmax
      )
  })
bar_data <- geom_data_processor(
  "bar",
  function(axis_l, method_tib_filt) {
    method_tib_filt %>%
      select_(ymin = "method_ymin", ymax = "method_ymax", col = axis_l$col, value = axis_l$value) %>%
      mutate(
        xmin = axis_l$xmin,
        xmax = axis_l$xmin + value * axis_l$xwidth
      )
  })
invbar_data <- geom_data_processor(
  "invbar",
  function(axis_l, method_tib_filt) {
    method_tib_filt %>%
      select_(ymin = "method_ymin", ymax = "method_ymax", col = axis_l$col, value = axis_l$value) %>%
      mutate(
        xmin = axis_l$xmax - value * axis_l$xwidth,
        xmax = axis_l$xmax
      )
  })
text_data <- geom_data_processor(
  "text",
  function(axis_l, method_tib_filt) {
    method_tib_filt %>%
      select_(y = "method_y", col = axis_l$col, label = axis_l$value) %>%
      mutate(
        x = axis_l$x
      )
  })
pie_data <- geom_data_processor(
  "pie",
  function(axis_l, method_tib_filt) {
    cols <- axis_l$col[[1]]
    values <- axis_l$value[[1]]
    method_tib_filt %>%
      select(method_short_name, y0 = method_y, one_of(values)) %>%
      gather(piece, value, one_of(values)) %>%
      arrange(method_short_name, piece) %>%
      group_by(method_short_name) %>%
      mutate(
        col = cols[piece],
        x0 = axis_l$x,
        pct = value / sum(value),
        rad = pct * 2 * pi,
        rad_end = cumsum(rad),
        rad_start = rad_end - rad,
        r0 = 0,
        r = row_height / 2
      ) %>%
      filter(rad_end != rad_start) %>%
      ungroup()
  })


# CREATE LEGENDS
legy_start <- min(method_tib$method_ymin)

leg_topinf <- data_frame(
  label = names(topinf_colours),
  col = topinf_colours,
  x = 1,
  y = seq_along(topinf_colours),
  explanation = c("inferred by algorithm", "determined by parameter", "fixed by algorithm")
)
leg_prior <- data_frame(
  label = c("id", "#", "id", "#"),
  type1 = c("optional_", "optional_", "required_", "required_"),
  col = prior_type1cols[type1],
  x = 1,
  y = seq_along(type1),
  explanation = c("optional cell/gene ids", "optional amount", "required cell/gene ids", "required amount")
)
leg_circles <- data_frame(
  r = c(.05, .1, .2, .4, .6, .8, 1)/2,
  x = cumsum(c(0, (.05 + r[-1] + r[-length(r)]))),
  colbench = colbench_fun(r*2),
  colqc = colqc_fun(r*2)
)

make_scale_legend <- function(scale, range_labels) {
  g <-
    ggplot() +
    geom_point(aes(x, y, col = z), data_frame(x = c(0, 1, runif(1000)), y = c(0, 1, runif(1000)), z = (x + y) / 2)) +
    scale_colour_gradientn(colours = scale, breaks = c(0, 1), labels = range_labels) +
    labs(colour = NULL) +
    theme(legend.position = "bottom")
  cowplot::get_legend(g)
}

bench_leg <- make_scale_legend(viridisLite::viridis(101, option = colbench), range_labels = c("low", "high"))
time_leg <- make_scale_legend(viridisLite::viridis(101, option = coltime), range_labels = c("long", "short"))
qc_leg <- make_scale_legend(viridisLite::viridis(101, option = colqc), range_labels = c("low", "high"))

error_leg_df <- data_frame(
  rad_start = seq(0, pi, length.out = 5)[-5],
  rad_end = seq(0, pi, length.out = 5)[-1],
  rad = (rad_start + rad_end) / 2,
  label = c("Memory limit exceeded", "Time limit exceeded", "Dataset-specific error", "Stochastic error"),
  fill = error_colours[c("pct_memory_exceeded", "pct_time_exceeded", "pct_allerrored", "pct_stochastic")],
  colour = rep("black", length(rad)),
  lab_x = row_height * sin(rad),
  lab_y = row_height * cos(rad),
  hjust = rep(0, length(rad)),
  vjust = seq(0, 1, length.out = length(rad)+2)[c(-1,-(length(rad)+2))]
)

# Stamp
stamp <- paste0("Generated on ", Sys.Date())

# MAKE PLOT
g1 <- ggplot(method_tib) +

  # THEME SETTINGS
  coord_equal(expand = FALSE) +
  scale_alpha_identity() +
  scale_colour_identity() +
  scale_fill_identity() +
  cowplot::theme_nothing() +

  # SEPARATOR LINES
  geom_rect(aes(xmin = axtr$name$xmin, xmax = axtr$qcpa$xmax, ymin = method_ymin-(spacing/2), ymax = method_ymax+(spacing/2)), method_tib %>% filter(method_i %% 2 == 1), fill = "#EEEEEE") +

  # METRIC AXIS
  geom_segment(aes(x = x, xend = x, y = -.3, yend = -.1), axis %>% filter(show_label), size = .5) +
  geom_text(aes(x = x, y = 0, label = label), axis %>% filter(show_label), angle = 30, vjust = 0, hjust = 0) +

  # GROUPING AXIS
  geom_segment(aes(x = xmin, xend = xmax, y = y, yend = y), grouping, size = 1) +
  geom_text(aes(x = x, y = y+.5, label = label), grouping, vjust = 1, hjust = 0.5, fontface = "bold") +
  geom_text(aes(x = xmin, y = y+.7, label = key), grouping %>% filter(key != ""), vjust = 1, hjust = 0, fontface = "bold", size = 5) +

  # METHOD NAMES
  geom_text(aes(x = axtr$name$xmax, y = method_y, label = method_name), hjust = 1, vjust = .5) +

  # INSUFFICIENT DATA LABEL
  # geom_text(aes(x = axtr$line$x, y = method_y, label = "insufficient data"), method_tib %>% filter(remove_results), hjust = .5, vjust = .5) +

  # BARS
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = col), bar_data, colour = "black", size = .25) +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = col), invbar_data, colour = "black", size = .25) +
  # RECTANGLES
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = col), rect_data, colour = "black", size = .25) +
  # CIRCLES
  ggforce::geom_circle(aes(x0 = x0, y0 = y0, fill = col, r = size), circle_data, size = .25) +
  # STARS
  ggforce::geom_arc_bar(aes(x0 = x0, y0 = y0, r0 = r0, r = r, start = rad_start, end = rad_end, fill = col), data = pie_data %>% filter(pct <= (1-1e-10)), size = .25) +
  ggforce::geom_circle(aes(x0 = x0, y0 = y0, r = r, fill = col), data = pie_data %>% filter(pct > (1-1e-10)), size = .25) +
  # ggforce::geom_arc_bar(aes(x0 = x0, y0 = y0, r0 = r0, r = r, start = rad_start, end = rad_end, fill = NA), data = pie_data, size = .25) +
  # geom_segment(aes(x = x0, xend = x0, y = y0 + r0, yend = y0 + r), data = pie_data, size = .25) +
  # TEXT
  geom_text(aes(x = x, y = y, label = label, colour = col), data = text_data, vjust = .5, hjust = .5) +

  # TRAJ TYPE MINIS
  geom_rect(aes(xmin = axtr$maxt$xmin, xmax = axtr$maxt$xmax, ymin = method_ymin, ymax = method_ymax, alpha = maxtraj_replace_id, fill = "#ABCDEF"), colour = "black", size = .25) +

  # PRIORS MINIS
  # geom_rect(aes(xmin = axtr$prio$xmin, xmax = axtr$prio$xmax, ymin = method_ymin, ymax = method_ymax, alpha = prior_replace_id, fill = "#ABCDEF"), colour = "black", size = .25) +

  # RESERVE SPACE
  expand_limits(x = c(max(axis$xmax)+2), y = legy_start - 4.1) +

  # LEGEND: TOPOLOGY
  geom_text(aes(3, legy_start - 1, label = "Topology constraints"), data_frame(i = 1), hjust = 0, vjust = 0, fontface = "bold") +
  geom_text(aes(3.5, legy_start - 1 - y * .7, label = label, colour = col), leg_topinf, hjust = 0, vjust = 0) +
  geom_text(aes(6.5, legy_start - 1 - y * .7, label = explanation), leg_topinf, hjust = 0, vjust = 0) +

  # LEGEND: PRIOR
  geom_text(aes(12.5, legy_start - 1, label = "Prior"), data_frame(i = 1), hjust = 0, vjust = 0, fontface = "bold") +
  geom_text(aes(13, legy_start - 1 - y * .7, label = label, colour = col), leg_prior, hjust = 0, vjust = 0) +
  geom_text(aes(14.5, legy_start - 1- y * .7, label = explanation), leg_prior, hjust = 0, vjust = 0) +

  # LEGEND: BENCHMARK
  geom_text(aes(20.5, legy_start - 1, label = "Benchmark score"), data_frame(i = 1), hjust = 0, vjust = 0, fontface = "bold") +
  ggforce::geom_circle(aes(x0 = 21.3 + x, y0 = legy_start - 2.3 + r, r = r, fill = colbench), size = .25, leg_circles) +
  geom_text(aes(x = 21.3 + x, y = legy_start - 2.3 - .4, label = c("low", "high")), leg_circles %>% slice(c(1, n()))) +
  # ggimage::geom_subview(x = 22, y = legy_start - 2, subview = bench_leg, width = 3, height = 1) +

  # LEGEND: PCT ERRORED
  geom_text(aes(26, legy_start - 1, label = "Error reason"), data_frame(i = 1), hjust = 0, vjust = 0, fontface = "bold") +
  ggforce::geom_arc_bar(aes(x0 = 26.5, y0 = legy_start - 2.5, r0 = 0, r = row_height*.75, start = rad_start, end = rad_end, fill = fill), size = .25, error_leg_df) +
  ggforce::geom_arc_bar(aes(x0 = 26.5, y0 = legy_start - 2.5, r0 = 0, r = row_height*.75, start = rad_start, end = rad_end, fill = NA), size = .25, error_leg_df) +
  geom_text(aes(x = 26.5 + lab_x + .5, y = legy_start - 2.5 + lab_y, label = label, vjust = vjust, hjust = hjust), error_leg_df) +
  geom_segment(aes(x = 26.5, xend = 26.5, y = legy_start - 2.5, yend = legy_start - 2.5 + row_height*.75), data = data_frame(z = 1), size = .25) +

  # LEGEND: QC SCORE
  geom_text(aes(34, legy_start - 1, label = "QC score"), data_frame(i = 1), hjust = 0, vjust = 0, fontface = "bold") +
  # ggimage::geom_subview(x = 40, y = legy_start - 2, subview = qc_leg, width = 3, height = 1)
  ggforce::geom_circle(aes(x0 = 34.8 + x, y0 = legy_start - 2.3 + r, r = r, fill = colqc), size = .25, leg_circles) +
  geom_text(aes(x = 34.8 + x, y = legy_start - 2.3 - .4, label = c("low", "high")), leg_circles %>% slice(c(1, n()))) +

  # GENERATION SENTENCE
  geom_text(aes(1, legy_start - 4, label = stamp), colour = "#cccccc", hjust = 0, vjust = 0) +
  expand_limits(y = legy_start - 4.3)


# WRITE FILES
overview_fig_file <- figure_file("overview.svg")
ggsave(overview_fig_file, g1, width = 17.5, height = 15)
xml2::read_xml(overview_fig_file) %>% replace_svg(minis) %>% xml2::write_xml(overview_fig_file)


