library(dynbenchmark)
library(tidyverse)
library(dynutils)

experiment("05-scaling")

list2env(read_rds(result_file("scaling.rds")), .GlobalEnv)

#' @examples
#' # examine some errors
#' data %>% filter(method_id == "calista", error_status == "method_error", nrow > 100, ncol > 100) %>% select(dataset_id, stdout) %>% pull(stdout)%>% head(5)
#' data %>% filter(method_id == "calista", error_status == "method_error") %>% pull(dataset_id) %>% head(5)
#' data %>% filter(method_id == "calista", error_status == "method_error") %>% mutate(txt = paste0(stdout, stderr, error_message)) %>% pull(txt) %>% head(5) %>% cat
#' data %>% filter(method_id == "dpt", error_status == "method_error") %>% mutate(txt = paste0(stdout, stderr, error_message)) %>% pull(txt) %>% head(5) %>% cat
#'
#' data <- readr::read_rds(dynbenchmark::derived_file("datasets.rds", experiment_id = "05-scaling")) %>% filter(id == "scaling_0001") %>% pull(fun) %>% first() %>% invoke()


axis_scale <- data_frame(lnrow = seq(1, 6), nrow = 10^lnrow)
method_ids <- unique(data$method_id) %>% setdiff("error")

##########################################################
###############  GENERATE SUMMARY FIGURE   ###############
##########################################################

models <-
  models %>%
  mutate(
    score = 1 - dyneval:::calculate_harmonic_mean(dynutils::scale_minmax(lpredtime), dynutils::scale_minmax(lpredmem), dynutils::scale_minmax(pct_errored))
  ) %>%
  # arrange(desc(score)) %>%
  arrange(method_id) %>%
  mutate(
    method_id_f = factor(method_id, levels = method_id),
    method_name_f = factor(method_name, levels = method_name),
    y = -as.integer(method_id_f)
  )

columns <-
  data_frame(
    id = c("score", "lpredtime", "time_lnrow", "time_lncol", "time_intercept", "lpredmem", "mem_lnrow", "mem_lncol", "mem_intercept", "pct_errored"),
    name = c("Score", "Predicted\nlog time", "Time coeff.\nintercept", "Time coeff.\n# cells", "Time coeff.\n# features", "Predicted\nlog memory", "Mem coeff.\nintercept", "Mem coeff.\n# cells", "Mem coeff.\n# features", "% errored"),
    fill = c("overall", "time", "time", "time", "time", "memory", "memory", "memory", "memory", "errors"),
    x = 1.1 * seq_along(id),
    min = map_dbl(id, ~ min(models[[.]])),
    max = map_dbl(id, ~ max(models[[.]]))
  )
barplot_data <-
  map_df(seq_len(nrow(columns)), function(i) {
    row <- columns %>% extract_row_to_list(i)

    df <- models %>%
      select(method_id, y, value = !!row$id) %>%
      mutate(
        column = row$id,
        value = (value - row$min) / (row$max - row$min),
        x = row$x,
        fill = row$fill
      )
  })


g <-
  ggplot() +
  geom_text(aes(1, y, label = method_name), models, hjust = 1) +
  geom_text(aes(x + .5, 2.5, label = name), columns, hjust = .5) +
  geom_text(aes(x, .5, label = round(min, 2)), columns, hjust = 0) +
  geom_text(aes(x + 1, .5, label = round(max, 2)), columns, hjust = 1) +
  geom_segment(aes(y = 0, yend = 0, x = x, xend = x + 1), columns) +
  geom_rect(aes(ymin = y - .45, ymax = y + .45, xmin = x, xmax = x + value, fill = fill), barplot_data) +
  theme_clean() +
  theme(
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    legend.position = "bottom"
  ) +
  expand_limits(x = -.5) +
  scale_fill_brewer(palette = "Dark2") +
  labs(
    title = "Analysis on scalability of TI methods",
    subtitle = "Summary of linear models trained on execution time and memory in function of dataset size",
    fill = "Type"
  )

g
ggsave(figure_file("ranking.svg"), g, width = 14, height = 12)


##########################################################
############### GENERATE INDIVIDUAL FIGURE ###############
##########################################################
plots <- map(method_ids, function(method_id) {
  data_method <- data %>% filter(method_id == !!method_id)
  model_method <- models %>% filter(method_id == !!method_id)
  data_noerror <- data_method %>% filter(error_status == "no_error")
  pred_method <- data_pred %>% filter(method_id == !!method_id)

  data_noerror <- bind_rows(
    data_noerror,
    data_noerror %>% group_by(method_id, lnrow, lncol) %>% select_if(is.numeric) %>% summarise_if(is.numeric, mean) %>% mutate(orig_dataset_id = "mean") %>% ungroup()
  )

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
  g1

  g2 <-
    ggplot(data_noerror) +
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
  g2

  g3 <-
    ggplot(data_noerror) +
    geom_tile(aes(lnrow, lncol), fill = "darkgray", data_method) +
    geom_tile(aes(lnrow, lncol, fill = log10(max_mem))) +
    scale_fill_distiller(palette = "RdYlBu") +
    scale_x_continuous(breaks = axis_scale$lnrow, labels = axis_scale$nrow) +
    scale_y_continuous(breaks = axis_scale$lnrow, labels = axis_scale$nrow) +
    theme_classic() +
    theme(legend.position = "bottom") +
    labs(x = "# cells", y = "# features", fill = "Log10(Max mem)") +
    facet_wrap(~ orig_dataset_id, ncol = 1) +
    coord_equal()
  g3

  g4a <-
    ggplot(pred_method) +
    geom_tile(aes(lnrow, lncol, fill = lpredtime)) +
    scale_fill_distiller(palette = "RdYlBu") +
    scale_x_continuous(breaks = axis_scale$lnrow, labels = axis_scale$nrow) +
    scale_y_continuous(breaks = axis_scale$lnrow, labels = axis_scale$nrow) +
    theme_classic() +
    theme(legend.position = "bottom") +
    labs(x = "# cells", y = "# features", fill = "Predicted time") +
    coord_equal()
  g4b <-
    ggplot(pred_method) +
    geom_tile(aes(lnrow, lncol, fill = lpredmem)) +
    scale_fill_distiller(palette = "RdYlBu") +
    scale_x_continuous(breaks = axis_scale$lnrow, labels = axis_scale$nrow) +
    scale_y_continuous(breaks = axis_scale$lnrow, labels = axis_scale$nrow) +
    theme_classic() +
    theme(legend.position = "bottom") +
    labs(x = "# cells", y = "# features", fill = "Predicted memory") +
    coord_equal()

  patchwork::wrap_plots(
    patchwork::wrap_plots(
      g1 + labs(title = "Execution information"),
      g2 + labs(title = "Timings grid"),
      g3 + labs(title = "Memory grid"),
      nrow = 1
    ),
    patchwork::wrap_plots(
      g4a + labs(title = "Predicted timings"),
      g4b + labs(title = "Predicted memory"),
      ncol = 1
    ),
    nrow = 1,
    widths = c(3, 1)
  ) +
    patchwork::plot_annotation(
      title = paste0("Scalability results for ", data_method$method_name[[1]]),
      theme = theme(title = element_text(size = 20))
    )
})

dir.create(figure_file("results"), showWarnings = FALSE)
pbapply::pblapply(seq_along(method_ids), cl = 8, function(i) {
  mid <- method_ids[[i]]
  pl <- plots[[i]]
  cat("Plotting ", mid, "\n", sep = "")
  ggsave(figure_file(c("results/", mid, ".svg")), pl, width = 16, height = 12)
})




##########################################################
###############     GENERATE ERROR LOGS    ###############
##########################################################
last_lines <- function(s, num) {
  s %>%
    stringr::str_split(pattern = "\n") %>%
    first() %>%
    keep(~ . != "") %>%
    tail(num) %>%
    paste(collapse = "\n")
}
cluster <- function(s, num_diffs) {
  clusters <- c(1, rep(NA, length(s) - 1))
  for (i in seq_len(length(s) - 1)) {
    cli <- clusters[seq_len(i)]
    ixs <- map_int(unique(cli), ~ first(which(cli == .)))
    dists <- utils::adist(s[ixs], s[i+1])[,1]
    ix <- ixs[dists <= num_diffs]
    clusters[[i+1]] <-
      if (length(ix) > 0) {
        cli[[ix[[1]]]]
      } else {
        max(clusters, na.rm = TRUE) + 1
      }
  }
  clusters
}


pbapply::pblapply(method_ids, cl = 8, function(mid) {
  selection <-
    data %>%
    filter(method_id == mid, error_status == "method_error") %>%
    mutate(
      error_text = paste0(stdout, "\n", stderr, "\n", error_message),
      error_truncated = map_chr(error_text, last_lines, num = 5)
    )

  if (nrow(selection) > 2) {
    clusts <- cluster(s = selection$error_truncated, num_diffs = 10)
  } else if (nrow(selection) == 1) {
    clusts <- 1
  } else {
    clusts <- c()
  }

  cluster_lines <- unlist(lapply(unique(clusts), function(cl) {
    ixs <- which(clusts == cl)
    ix <- ixs %>% first()
    vals <- selection %>% dynutils::extract_row_to_list(ix)
    c(
      "## ERROR CLUSTER ", cl, "\n",
      "\n",
      " * Number of instances: ", length(ixs), "\n",
      " * Dataset ids: ", paste(selection$dataset_id[ixs], collapse = ", "), "\n",
      "\n",
      "Last 5 lines of ", selection$dataset_id[ix], ":\n",
      "```\n",
      vals$error_truncated, "\n",
      "```\n",
      "\n"
    )
  }))

  lines <- paste0(c(
    "# ", mid, "\n",
    "![Overview](", mid, ".svg)\n",
    "\n",
    cluster_lines
  ), collapse = "")

  readr::write_lines(lines, figure_file(c("results/", mid, "_overview.md")))

  invisible()
})
