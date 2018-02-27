library(dynalysis)
library(tidyverse)
library(dynplot)

experiment("9-main_figure")

method_tib <- read_rds(result_file("method_tib.rds"))

method_ord <- method_tib %>% arrange(desc(harm_mean)) %>% .$method_name

# coord calculation
row_height <- .95

method_tib <- method_tib %>%
  mutate(
    method_name_f = factor(method_name, levels = method_ord),
    method_y = -as.integer(method_name_f),
    method_ymin = method_y - row_height / 2,
    method_ymax = method_y + row_height / 2
  ) %>%
  mutate_at(
    c(
      "harm_mean", "rank_correlation", "rank_edge_flip", "rank_rf_mse", "source_real", "source_synthetic",
      "trajtype_bifurcation", "trajtype_convergence", "trajtype_directed_acyclic_graph", "trajtype_directed_cycle",
      "trajtype_directed_graph", "trajtype_directed_linear", "trajtype_multifurcation", "trajtype_rooted_tree"
    ),
    funs(sc = . / max(.))
  )

# axis info
axis <-
  tribble(
    ~id,       ~label,           ~xsep, ~xwidth, ~line,  ~ticks,
    "harm",    "Overall score",  0.0,   5,       TRUE,   list("0" = 0, "0.6" = 3),
    "corr",    "Correlation",    0.5,   1,       FALSE,  list(),
    "edge",    "Edge flip",      0.1,   1,       FALSE,  list(),
    "rfms",    "RF MSE",         0.1,   1,       FALSE,  list(),
    "real",    "Real",           0.5,   1,       FALSE,  list(),
    "synt",    "Synthetic",      0.1,   1,       FALSE,  list(),
    "line",    "Linear",         0.5,   1,       FALSE,  list(),
    "bifu",    "Bifurcation",    0.1,   1,       FALSE,  list(),
    "conv",    "Convergence",    0.1,   1,       FALSE,  list(),
    "cycl",    "Cycle",          0.1,   1,       FALSE,  list(),
    "mult",    "Multifurcation", 0.1,   1,       FALSE,  list(),
    "root",    "Rooted tree",    0.1,   1,       FALSE,  list(),
    "dagg",    "DAG",            0.1,   1,       FALSE,  list(),
    "grap",    "Graph",          0.1,   1,       FALSE,  list()
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
    ~label, ~xmin, ~xmax,
    "Per metric", axtr$corr$xmin, axtr$rfms$xmax,
    "Per source", axtr$real$xmin, axtr$synt$xmax,
    "Per trajectory type", axtr$line$xmin, axtr$grap$xmax
  ) %>%
  mutate(
    x = (xmin + xmax) / 2
  )

# MAKE PLOT
g <- ggplot(method_tib) +

  # METRIC AXIS
  # geom_segment(aes(x = xmin, xend = xmax, y = 0, yend = 0), axis %>% filter(line), size = .5) +
  geom_segment(aes(x = x, xend = x, y = -.3, yend = -.1), axis, size = .5) +
  geom_text(aes(x = x, y = 0, label = label), axis, angle = 30, vjust = 0, hjust = 0) +
  expand_limits(y = 2) +

  # GROUPING AXIS
  geom_segment(aes(x = xmin, xend = xmax, y = 2, yend = 2), grouping, size = 1) +
  geom_text(aes(x = x, y = 2.2, label = label), grouping, vjust = 0, hjust = 0.5, fontface = "bold") +
  expand_limits(y = 3) +

  # METHOD NAMES
  geom_text(aes(x = -.1, y = method_y, label = method_name), hjust = 1, vjust = .5) +

  # OVERALL
  geom_rect(aes(xmin = axtr$harm$xmin, xmax = axtr$harm$xmin + harm_mean_sc * axtr$harm$xwidth, ymin = method_ymin, ymax = method_ymax, fill = harm_mean_sc)) +

  # PER METRIC
  geom_rect(aes(xmin = axtr$corr$xmin, xmax = axtr$corr$xmax, ymin = method_ymin, ymax = method_ymax, fill = rank_correlation_sc)) +
  geom_rect(aes(xmin = axtr$edge$xmin, xmax = axtr$edge$xmax, ymin = method_ymin, ymax = method_ymax, fill = rank_edge_flip_sc)) +
  geom_rect(aes(xmin = axtr$rfms$xmin, xmax = axtr$rfms$xmax, ymin = method_ymin, ymax = method_ymax, fill = rank_rf_mse_sc)) +

  # PER SOURCE
  geom_rect(aes(xmin = axtr$real$xmin, xmax = axtr$real$xmax, ymin = method_ymin, ymax = method_ymax, fill = source_real_sc)) +
  geom_rect(aes(xmin = axtr$synt$xmin, xmax = axtr$synt$xmax, ymin = method_ymin, ymax = method_ymax, fill = source_synthetic_sc)) +

  # PER TRAJ TYPE
  geom_rect(aes(xmin = axtr$line$xmin, xmax = axtr$line$xmax, ymin = method_ymin, ymax = method_ymax, fill = trajtype_directed_linear_sc)) +
  geom_rect(aes(xmin = axtr$bifu$xmin, xmax = axtr$bifu$xmax, ymin = method_ymin, ymax = method_ymax, fill = trajtype_bifurcation_sc)) +
  geom_rect(aes(xmin = axtr$conv$xmin, xmax = axtr$conv$xmax, ymin = method_ymin, ymax = method_ymax, fill = trajtype_convergence_sc)) +
  geom_rect(aes(xmin = axtr$cycl$xmin, xmax = axtr$cycl$xmax, ymin = method_ymin, ymax = method_ymax, fill = trajtype_directed_cycle_sc)) +
  geom_rect(aes(xmin = axtr$mult$xmin, xmax = axtr$mult$xmax, ymin = method_ymin, ymax = method_ymax, fill = trajtype_multifurcation_sc)) +
  geom_rect(aes(xmin = axtr$root$xmin, xmax = axtr$root$xmax, ymin = method_ymin, ymax = method_ymax, fill = trajtype_rooted_tree_sc)) +
  geom_rect(aes(xmin = axtr$dagg$xmin, xmax = axtr$dagg$xmax, ymin = method_ymin, ymax = method_ymax, fill = trajtype_directed_acyclic_graph_sc)) +
  geom_rect(aes(xmin = axtr$grap$xmin, xmax = axtr$grap$xmax, ymin = method_ymin, ymax = method_ymax, fill = trajtype_directed_graph_sc)) +

  # RESERVE SPACE
  expand_limits(x = c(-6, 40)) +

  # THEME SETTINGS
  coord_equal(expand = FALSE) +
  viridis::scale_fill_viridis(option = "B") +
  cowplot::theme_nothing()

ggsave(figure_file("overview.pdf"), g, width = 20, height = 10)

