library(dynalysis)
library(tidyverse)
library(dynplot)

experiment("9-main_figure")

method_tib <- read_rds(result_file("method_tib.rds"))

method_ord <- method_tib %>% arrange(desc(harm_mean)) %>% .$method_name

# determine palettes
continuous_scale <- viridisLite::viridis(101, option = "B")
sc_fun <- function(x) {
  x / max(x, na.rm = T)
}
sc_col_fun <- function(x) {
  sc <- sc_fun(x)
  ifelse(is.na(sc), "darkgray", continuous_scale[round(sc * 100)+1])
}

error_colours <- setNames(RColorBrewer::brewer.pal(3, "Set2"), c("pct_inside", "pct_memory_exceeded", "pct_time_exceeded"))

prior_colours <- setNames(RColorBrewer::brewer.pal(5, "Set1"), c("grouping_assignment", "grouping_assignment;start_cells;n_end_states", "marker_feature_ids", "n_end_states", "start_cells"))

topinf_colours <- setNames(RColorBrewer::brewer.pal(3, "Set3"), c("free", "parameter", "fixed"))

trafo_colours <- setNames(RColorBrewer::brewer.pal(6, "Set3")[4:6], c("minimal", "moderate", "extensive"))

maxtraj_colours <- setNames(trajectory_types$color, trajectory_types$id)

# coord calculation
row_height <- .95

method_tib <- method_tib %>%
  mutate(
    method_name_f = factor(method_name, levels = method_ord),
    method_y = -as.integer(method_name_f),
    method_ymin = method_y - row_height / 2,
    method_ymax = method_y + row_height / 2,
    scalability_nfeat = ifelse(complexity_inferred == "nothing", NA, ifelse(grepl("nfeat2", complexity_inferred), 1, ifelse(grepl("nfeat", complexity_inferred), 2, 3))),
    scalability_ncell = ifelse(complexity_inferred == "nothing", NA, ifelse(grepl("ncell2", complexity_inferred), 1, ifelse(grepl("ncell", complexity_inferred), 2, 3))),
    rank_time_method = percent_rank(-time_method),
    rank_mean_var = percent_rank(-mean_var),
    rank_sets_seeds = ifelse(sets_seeds, 1, 2),
    rank_citations = percent_rank(Citations),
    trafo = str_replace(output_transformation, ":.*", "")
  ) %>%
  mutate_at(
    c(
      "harm_mean", "rank_correlation", "rank_edge_flip", "rank_rf_mse", "source_real", "source_synthetic",
      "trajtype_bifurcation", "trajtype_convergence", "trajtype_directed_acyclic_graph", "trajtype_directed_cycle",
      "trajtype_directed_graph", "trajtype_directed_linear", "trajtype_multifurcation", "trajtype_rooted_tree",
      "qc_score", "qc_availability", "qc_behaviour", "qc_code_assurance", "qc_code_quality", "qc_documentation", "qc_paper",
      "rank_time_method", "scalability_nfeat", "scalability_ncell", "rank_mean_var", "rank_sets_seeds", "rank_citations"
    ),
    funs(sc = sc_fun, sc_col = sc_col_fun)
  ) %>%
  mutate(
    prior_colour = prior_colours[prior_str],
    topinf_colour = topinf_colours[topology_inference_type],
    trafo_colour = trafo_colours[trafo],
    maxtraj_colour = maxtraj_colours[maximal_trajectory_types]
  )

error_reasons <-
  method_tib %>%
  select(method_short_name, method_y, pct_errored, pct_memory_exceeded, pct_time_exceeded) %>%
  mutate(pct_inside = pct_errored - pct_memory_exceeded - pct_time_exceeded) %>%
  select(-pct_errored) %>%
  gather(reason, percentage, -method_short_name, -method_y) %>%
  arrange(method_short_name, reason) %>%
  group_by(method_short_name) %>%
  mutate(
    rad = percentage * 2 * pi,
    rad_end = cumsum(rad),
    rad_start = rad_end - rad
  ) %>%
  ungroup() %>%
  mutate(
    error_colour = error_colours[reason]
  )

# axis info
axis <-
  tribble(
    ~id,       ~label,              ~xsep, ~xwidth, ~line,  ~ticks,
    "harm",    "Score",             0.0,   5,       TRUE,   list(),

    "corr",    "Correlation",       0.5,   1,       FALSE,  list(),
    "edge",    "Edge flip",         0.1,   1,       FALSE,  list(),
    "rfms",    "RF MSE",            0.1,   1,       FALSE,  list(),

    "real",    "Real",              0.5,   1,       FALSE,  list(),
    "synt",    "Synthetic",         0.1,   1,       FALSE,  list(),

    "line",    "Linear",            0.5,   1,       FALSE,  list(),
    "bifu",    "Bifurcation",       0.1,   1,       FALSE,  list(),
    "conv",    "Convergence",       0.1,   1,       FALSE,  list(),
    "cycl",    "Cycle",             0.1,   1,       FALSE,  list(),
    "mult",    "Multifurcation",    0.1,   1,       FALSE,  list(),
    "root",    "Rooted tree",       0.1,   1,       FALSE,  list(),
    "dagg",    "DAG",               0.1,   1,       FALSE,  list(),
    "grap",    "Graph",             0.1,   1,       FALSE,  list(),

    "time",    "Execution time",    0.5,   1,       FALSE,  list(),
    "nfea",    "Gene scalability",  0.1,   1,       FALSE,  list(),
    "ncel",    "Cell scalability",  0.1,   1,       FALSE,  list(),

    "vari",    "Robustness" ,       0.5,   1,       FALSE,  list(),
    "seed",    "Set no seeds",      0.1,   1,       FALSE,  list(),
    "erro",    "% Errored",         0.1,   1,       FALSE,  list(),
    "prio",    "Required prior",    0.1,   1,       FALSE,  list(),
    "topo",    "Topology inference", .1,   1,       FALSE,  list(),
    "traf",    "Transformation",    0.1,   1,       FALSE,  list(),
    "maxt",    "Maximal traj. type", .1,   1,       FALSE,  list(),
    "cite",    "# Citations",       0.1,   1,       FALSE,  list(),

    "qcsc",    "Score",             0.5,   5,       FALSE,  list(),
    "qcav",    "Availability",      0.1,   1,       FALSE,  list(),
    "qcbe",    "Behaviour",         0.1,   1,       FALSE,  list(),
    "qcca",    "Code assurance",    0.1,   1,       FALSE,  list(),
    "qccq",    "Code quality",      0.1,   1,       FALSE,  list(),
    "qcdo",    "Documentation",     0.1,   1,       FALSE,  list(),
    "qcpa",    "Paper",             0.1,   1,       FALSE,  list()
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

axis

grouping <-
  tribble(
    ~label, ~y, ~xmin, ~xmax,
    "Benchmark pipeline", 3, axtr$harm$xmin, axtr$ncel$xmax,
    "Per metric", 2, axtr$corr$xmin, axtr$rfms$xmax,
    "Per source", 2, axtr$real$xmin, axtr$synt$xmax,
    "Per trajectory type", 2, axtr$line$xmin, axtr$grap$xmax,
    "Timings", 2, axtr$time$xmin, axtr$ncel$xmax,
    "Varia", 3, axtr$vari$xmin, axtr$cite$xmax,
    "Quality control", 3, axtr$qcsc$xmin, axtr$qcpa$xmax
  ) %>%
  mutate(
    x = (xmin + xmax) / 2
  )

# MAKE PLOT
g <- ggplot(method_tib) +

  # METRIC AXIS
  geom_segment(aes(x = x, xend = x, y = -.3, yend = -.1), axis, size = .5) +
  geom_text(aes(x = x, y = 0, label = label), axis, angle = 30, vjust = 0, hjust = 0) +
  expand_limits(y = 2) +

  # GROUPING AXIS
  geom_segment(aes(x = xmin, xend = xmax, y = y, yend = y), grouping, size = 1) +
  geom_text(aes(x = x, y = y+.2, label = label), grouping, vjust = 0, hjust = 0.5, fontface = "bold") +
  expand_limits(y = 4) +

  # METHOD NAMES
  geom_text(aes(x = -.1, y = method_y, label = method_name), hjust = 1, vjust = .5) +

  # OVERALL
  geom_rect(aes(xmin = axtr$harm$xmin, xmax = axtr$harm$xmin + harm_mean_sc * axtr$harm$xwidth, ymin = method_ymin, ymax = method_ymax, fill = harm_mean_sc_col)) +

  # PER METRIC
  geom_rect(aes(xmin = axtr$corr$xmin, xmax = axtr$corr$xmax, ymin = method_ymin, ymax = method_ymax, fill = rank_correlation_sc_col)) +
  geom_rect(aes(xmin = axtr$edge$xmin, xmax = axtr$edge$xmax, ymin = method_ymin, ymax = method_ymax, fill = rank_edge_flip_sc_col)) +
  geom_rect(aes(xmin = axtr$rfms$xmin, xmax = axtr$rfms$xmax, ymin = method_ymin, ymax = method_ymax, fill = rank_rf_mse_sc_col)) +

  # PER SOURCE
  geom_rect(aes(xmin = axtr$real$xmin, xmax = axtr$real$xmax, ymin = method_ymin, ymax = method_ymax, fill = source_real_sc_col)) +
  geom_rect(aes(xmin = axtr$synt$xmin, xmax = axtr$synt$xmax, ymin = method_ymin, ymax = method_ymax, fill = source_synthetic_sc_col)) +

  # PER TRAJ TYPE
  geom_rect(aes(xmin = axtr$line$xmin, xmax = axtr$line$xmax, ymin = method_ymin, ymax = method_ymax, fill = trajtype_directed_linear_sc_col)) +
  geom_rect(aes(xmin = axtr$bifu$xmin, xmax = axtr$bifu$xmax, ymin = method_ymin, ymax = method_ymax, fill = trajtype_bifurcation_sc_col)) +
  geom_rect(aes(xmin = axtr$conv$xmin, xmax = axtr$conv$xmax, ymin = method_ymin, ymax = method_ymax, fill = trajtype_convergence_sc_col)) +
  geom_rect(aes(xmin = axtr$cycl$xmin, xmax = axtr$cycl$xmax, ymin = method_ymin, ymax = method_ymax, fill = trajtype_directed_cycle_sc_col)) +
  geom_rect(aes(xmin = axtr$mult$xmin, xmax = axtr$mult$xmax, ymin = method_ymin, ymax = method_ymax, fill = trajtype_multifurcation_sc_col)) +
  geom_rect(aes(xmin = axtr$root$xmin, xmax = axtr$root$xmax, ymin = method_ymin, ymax = method_ymax, fill = trajtype_rooted_tree_sc_col)) +
  geom_rect(aes(xmin = axtr$dagg$xmin, xmax = axtr$dagg$xmax, ymin = method_ymin, ymax = method_ymax, fill = trajtype_directed_acyclic_graph_sc_col)) +
  geom_rect(aes(xmin = axtr$grap$xmin, xmax = axtr$grap$xmax, ymin = method_ymin, ymax = method_ymax, fill = trajtype_directed_graph_sc_col)) +

  # TIMINGS
  geom_rect(aes(xmin = axtr$time$xmin, xmax = axtr$time$xmax, ymin = method_ymin, ymax = method_ymax, fill = rank_time_method_sc_col)) +
  geom_rect(aes(xmin = axtr$nfea$xmin, xmax = axtr$nfea$xmax, ymin = method_ymin, ymax = method_ymax, fill = scalability_nfeat_sc_col)) +
  geom_rect(aes(xmin = axtr$ncel$xmin, xmax = axtr$ncel$xmax, ymin = method_ymin, ymax = method_ymax, fill = scalability_ncell_sc_col)) +

  # VARIA
  geom_rect(aes(xmin = axtr$vari$xmin, xmax = axtr$vari$xmax, ymin = method_ymin, ymax = method_ymax, fill = rank_mean_var_sc_col)) +
  geom_rect(aes(xmin = axtr$seed$xmin, xmax = axtr$seed$xmax, ymin = method_ymin, ymax = method_ymax, fill = rank_sets_seeds_sc_col)) +
  ggforce::geom_arc_bar(aes(x0 = axtr$erro$x, y0 = method_y, r0 = 0, r = .45, start = rad_start, end = rad_end, fill = error_colour), data = error_reasons) +
  geom_rect(aes(xmin = axtr$prio$xmin, xmax = axtr$prio$xmax, ymin = method_ymin, ymax = method_ymax, fill = prior_colour)) +
  geom_rect(aes(xmin = axtr$topo$xmin, xmax = axtr$topo$xmax, ymin = method_ymin, ymax = method_ymax, fill = topinf_colour)) +
  geom_rect(aes(xmin = axtr$traf$xmin, xmax = axtr$traf$xmax, ymin = method_ymin, ymax = method_ymax, fill = trafo_colour)) +
  geom_rect(aes(xmin = axtr$maxt$xmin, xmax = axtr$maxt$xmax, ymin = method_ymin, ymax = method_ymax, fill = maxtraj_colour)) +
  geom_rect(aes(xmin = axtr$cite$xmin, xmax = axtr$cite$xmax, ymin = method_ymin, ymax = method_ymax, fill = rank_citations_sc_col)) +

  # QUALITY CONTROL
  geom_rect(aes(xmin = axtr$qcsc$xmin, xmax = axtr$qcsc$xmin + qc_score_sc * axtr$qcsc$xwidth, ymin = method_ymin, ymax = method_ymax, fill = qc_score_sc_col)) +
  geom_rect(aes(xmin = axtr$qcav$xmin, xmax = axtr$qcav$xmax, ymin = method_ymin, ymax = method_ymax, fill = qc_availability_sc_col)) +
  geom_rect(aes(xmin = axtr$qcbe$xmin, xmax = axtr$qcbe$xmax, ymin = method_ymin, ymax = method_ymax, fill = qc_behaviour_sc_col)) +
  geom_rect(aes(xmin = axtr$qcca$xmin, xmax = axtr$qcca$xmax, ymin = method_ymin, ymax = method_ymax, fill = qc_code_assurance_sc_col)) +
  geom_rect(aes(xmin = axtr$qccq$xmin, xmax = axtr$qccq$xmax, ymin = method_ymin, ymax = method_ymax, fill = qc_code_quality_sc_col)) +
  geom_rect(aes(xmin = axtr$qcdo$xmin, xmax = axtr$qcdo$xmax, ymin = method_ymin, ymax = method_ymax, fill = qc_documentation_sc_col)) +
  geom_rect(aes(xmin = axtr$qcpa$xmin, xmax = axtr$qcpa$xmax, ymin = method_ymin, ymax = method_ymax, fill = qc_paper_sc_col)) +

  # RESERVE SPACE
  expand_limits(x = c(-6, 47)) +

  # THEME SETTINGS
  coord_equal(expand = FALSE) +
  scale_fill_identity() +
  cowplot::theme_nothing()

ggsave(figure_file("overview.pdf"), g, width = 16, height = 10)

