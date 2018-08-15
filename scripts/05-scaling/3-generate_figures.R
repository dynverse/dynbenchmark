library(dynbenchmark)
library(tidyverse)
library(dynutils)
library(survival)

experiment("05-scaling")

list2env(read_rds(result_file("scaling.rds")), .GlobalEnv)

#' @examples
#' # examine some errors
#' execution_output %>% filter(method_id == "mpath", error_status == "method_error") %>% mutate(txt = paste0(stdout, stderr, error_message)) %>% pull(txt) %>% head(5) %>% cat
#' execution_output %>% filter(method_id == "dpt", error_status == "method_error") %>% mutate(txt = paste0(stdout, stderr, error_message)) %>% pull(txt) %>% head(5) %>% cat

axis_scale <- data_frame(lnrow = seq(1, 6), nrow = 10^lnrow)
method_ids <- unique(data$method_id) %>% setdiff("error")

##########################################################
###############       CREATE FIGURES       ###############
##########################################################

columns <-
  data_frame(
    id = c("lpredtime", "pct_errored", "lnrow", "lncol", "intercept"),
    name = c("Predicted\nlog time", "% errored", "Coefficient\nintercept", "Coefficient\n# cells", "Coefficient\n# features"),
    fill = c(NA, NA, NA, NA, NA),
    # fill = c(NA, NA, "p_lnrow", "p_lncol", "p_intercept"),
    x = 1.1 * seq_along(id),
    min = map_dbl(id, ~ min(execution_time_models[[.]])),
    max = map_dbl(id, ~ max(execution_time_models[[.]]))
  )
barplot_data <-
  map_df(seq_len(nrow(columns)), function(i) {
    row <- columns %>% extract_row_to_list(i)

    colids <- c(row$id, row$fill) %>% set_names("value", "fill") %>%  na.omit
    df <- execution_time_models %>%
      select(method_id, y, one_of(colids)) %>%
      mutate(column = row$id)
    colnames(df) <- c("method_id", "y", names(colids), "column")

    df %>%
      mutate(value = (value - row$min) / (row$max - row$min)) %>%
      mutate(x = row$x)
  })


g <-
  ggplot() +
  geom_text(aes(1, y, label = method_name), execution_time_models, hjust = 1) +
  geom_text(aes(x + .5, 2.5, label = name), columns, hjust = .5) +
  geom_text(aes(x, .5, label = round(min, 2)), columns, hjust = 0) +
  geom_text(aes(x + 1, .5, label = round(max, 2)), columns, hjust = 1) +
  geom_segment(aes(y = 0, yend = 0, x = x, xend = x + 1), columns) +
  geom_rect(aes(ymin = y - .45, ymax = y + .45, xmin = x, xmax = x + value), barplot_data) +
  theme_clean() +
  theme(
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank()
  ) +
  expand_limits(x = -.5) +
  scale_fill_distiller(palette = "RdYlBu") +
  labs(fill = "Coefficient\n-log pvalue")

g
ggsave(figure_file("ranking.svg"), g, width = 10, height = 10)

g <-
  execution_time_models %>%
  select(method_id, lpredtime, lnrow, lncol, intercept, pct_errored, pct_timelimit) %>%
  gather(feature, value, -method_id, -lpredtime) %>%
  ggplot(aes(value, lpredtime)) +
  ggrepel::geom_text_repel(aes(label = method_id), size = 3, colour = "darkgray") +
  geom_point() +
  theme_classic() +
  scale_colour_distiller(palette = "RdYlBu") +
  theme(legend.position = "bottom") +
  labs(y = "Predicted time", x = "Facet value") +
  facet_wrap(~feature, scales = "free", nrow = 2)

g
ggsave(figure_file("overview.svg"), g, width = 18, height = 12)



plots <- map(method_ids, function(method_id) {
  data_method <- data %>% filter(method_id == !!method_id)
  model_method <- execution_time_models %>% filter(method_id == !!method_id)
  timings_method <- data_method %>% filter(error_status == "no_error")
  pred_method <- data_pred %>% filter(method_id == !!method_id)

  g1 <-
    ggplot(data_method) +
    geom_tile(aes(lnrow, lncol, fill = error_status)) +
    scale_fill_manual(values = dynbenchmark::method_status_colours) +
    scale_x_continuous(breaks = axis_scale$lnrow, labels = axis_scale$nrow) +
    scale_y_continuous(breaks = axis_scale$lnrow, labels = axis_scale$nrow) +
    theme_classic() +
    theme(legend.position = "bottom") +
    labs(x = "# cells", y = "# features", fill = "Status") +
    facet_wrap(~ orig_dataset_id, ncol = 1) +
    coord_equal()

  if (nrow(model_method) > 0) {
    g2 <-
      ggplot(timings_method, aes(lnrow, time_method, group = paste0(orig_dataset_id, "__", lncol), colour = lncol)) +
      geom_point() +
      geom_line() +
      scale_x_continuous(breaks = axis_scale$lnrow, labels = axis_scale$nrow) +
      scale_y_log10(breaks = 10^seq(-5, 9)) +
      scale_colour_distiller(palette = "RdYlBu") +
      theme_classic() +
      theme(legend.position = "bottom") +
      labs(x = "# cells", y = "Log10(Time)", colour = "# features") +
      facet_wrap(~ orig_dataset_id, ncol = 1)

    g3 <-
      ggplot(timings_method, aes(lncol, time_method, group = paste0(orig_dataset_id, "__", lnrow), colour = lnrow)) +
      geom_point() +
      geom_line() +
      scale_x_continuous(breaks = axis_scale$lnrow, labels = axis_scale$nrow) +
      scale_y_log10(breaks = 10^seq(-5, 9)) +
      scale_colour_distiller(palette = "RdYlBu") +
      theme_classic() +
      theme(legend.position = "bottom") +
      labs(x = "# features", y = "Log10(Time)", colour = "# cells") +
      facet_wrap(~ orig_dataset_id, ncol = 1)

    g4 <-
      ggplot(timings_method) +
      geom_tile(aes(lnrow, lncol), fill = "darkgray", data_method) +
      geom_tile(aes(lnrow, lncol, fill = log10(time_method))) +
      scale_fill_distiller(palette = "RdYlBu") +
      scale_x_continuous(breaks = axis_scale$lnrow, labels = axis_scale$nrow) +
      scale_y_continuous(breaks = axis_scale$lnrow, labels = axis_scale$nrow) +
      theme_classic() +
      theme(legend.position = "bottom") +
      labs(x = "# cells", y = "# features", fill = "Log10(Time)") +
      facet_wrap(~ orig_dataset_id, ncol = 1) +
      coord_equal()

    g5 <-
      ggplot(pred_method) +
      geom_tile(aes(lnrow, lncol, fill = lpredtime)) +
      scale_fill_distiller(palette = "RdYlBu") +
      scale_x_continuous(breaks = axis_scale$lnrow, labels = axis_scale$nrow) +
      scale_y_continuous(breaks = axis_scale$lnrow, labels = axis_scale$nrow) +
      theme_classic() +
      theme(legend.position = "bottom") +
      labs(x = "# cells", y = "# features", fill = "Log10(Pred. Time)") +
      coord_equal()
  } else {
    g2 <- patchwork::plot_spacer()
    g3 <- patchwork::plot_spacer()
    g4 <- patchwork::plot_spacer()
    g5 <- patchwork::plot_spacer()
  }

  patchwork::wrap_plots(
    patchwork::wrap_plots(
      g1 + labs(title = "Execution information"),
      g2 + labs(title = "Time versus # cells"),
      g3 + labs(title = "Time versus # genes"),
      g4 + labs(title = "Timings grid"),
      nrow = 1
    ),
    g5 + labs(title = "Predicted timings"),
    nrow = 1,
    widths = c(4, 1)
  ) +
    patchwork::plot_annotation(
      title = paste0("Scalability results for ", data_method$method_name[[1]]),
      theme = theme(title = element_text(size = 20))
    )
})

dir.create(derived_file("results"), showWarnings = FALSE)
pbapply::pblapply(seq_along(method_ids), cl = 8, function(i) {
  mid <- method_ids[[i]]
  pl <- plots[[i]]
  cat("Plotting ", mid, "\n", sep = "")
  ggsave(derived_file(c("results/", mid, ".svg")), pl, width = 15, height = 12)
})

# compress to figure_file("results.tar.xz")
# unzip(figure_file("results.tar.xz"), exdir = derived_file("results"), overwrite = TRUE)
