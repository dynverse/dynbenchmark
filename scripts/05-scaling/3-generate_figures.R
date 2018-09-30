library(dynbenchmark)
library(tidyverse)
library(dynutils)

experiment("05-scaling")

list2env(read_rds(result_file("scaling.rds")), .GlobalEnv)

# check which datasets are available on the remote (some may not have executed entirely)
scaling_avail <- qsub::ls_remote(derived_file("", experiment = "05-scaling/dataset", remote = TRUE), remote = TRUE) %>% gsub("\\.rds$", "", .)

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
    score = 1 - dyneval:::calculate_arithmetic_mean(dynutils::scale_minmax(time_lpred), dynutils::scale_minmax(mem_lpred), dynutils::scale_minmax(pct_errored))
  ) %>%
  bind_rows(data %>% select(method_id, method_name) %>% distinct() %>% filter(!method_id %in% models$method_id) %>% mutate(score = 0, pct_errored = 1)) %>%
  arrange(desc(score)) %>%
  # arrange(method_id) %>%
  mutate(
    method_id_f = factor(method_id, levels = method_id),
    method_name_f = factor(method_name, levels = method_name),
    y = -as.integer(method_id_f)
  )

columns <- tribble(
  ~id, ~name, ~fill,
  "score", "Score", "overall",
  "time_lpred", "Prediced\nlog time", "time",
  "time_intercept", "Time coeff.\nintercept", "time",
  "time_lnrow", "Time coeff.\n# cells", "time",
  "time_lncol", "Time coeff.\n# features", "time",
  "mem_lpred", "Predicted\nlog memory", "memory",
  "mem_intercept", "Mem coeff.\nintercept", "memory",
  "mem_lnrow", "Mem coeff.\n# cells", "memory",
  "mem_lncol", "Mem coeff.\n# features", "memory",
  "pct_errored", "& errored", "errors"
) %>% mutate(
    x = 1.1 * seq_along(id),
    min = map_dbl(id, ~ min(models[[.]], na.rm = TRUE)),
    max = map_dbl(id, ~ max(models[[.]], na.rm = TRUE))
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
  geom_text(aes(1, y, label = method_name), models %>% select(-model_time, -model_mem), hjust = 1) +
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
ggsave(result_file("ranking.svg"), g, width = 14, height = 12)

write_rds(g, result_file("ranking.rds"))


##########################################################
############### GENERATE INDIVIDUAL FIGURE ###############
##########################################################
plots <- map(method_ids, function(method_id) {
  data_method <- data %>% filter(method_id == !!method_id)
  model_method <- models %>% filter(method_id == !!method_id)
  data_noerror <- data_method %>% filter(error_status == "no_error")
  data_error <- data_method %>% filter(error_status != "no_error")
  pred_method <- data_pred %>% filter(method_id == !!method_id)

  data_noerror <- bind_rows(
    data_noerror,
    data_noerror %>% group_by(method_id, lnrow, lncol) %>% select_if(is.numeric) %>% summarise_if(is.numeric, mean, na.rm = TRUE) %>% mutate(orig_dataset_id = "mean") %>% ungroup()
  )
  data_error <- bind_rows(
    data_error,
    data_error %>% group_by(method_id, lnrow, lncol) %>% select_if(is.numeric) %>% summarise_if(is.numeric, mean, na.rm = TRUE) %>% mutate(orig_dataset_id = "mean") %>% ungroup()
  )

  g1 <-
    ggplot(data_method) +
    geom_rect(aes(xmin = lnrow - .1, xmax = lnrow + .1, ymin = lncol - .1, ymax = lncol + .1, fill = error_status)) +
    scale_fill_manual(values = dynbenchmark::method_status_colours) +
    scale_x_continuous(breaks = axis_scale$lnrow, labels = axis_scale$nrow) +
    scale_y_continuous(breaks = axis_scale$lnrow, labels = axis_scale$nrow) +
    theme_classic() +
    theme(legend.position = "bottom") +
    labs(x = "# cells", y = "# features", fill = "Status") +
    facet_wrap(~ orig_dataset_id, ncol = 1) +
    coord_equal()
  # g1

  if (nrow(data_noerror) > 0) {
    g2 <-
      ggplot(data_noerror) +
      geom_rect(aes(xmin = lnrow - .1, xmax = lnrow + .1, ymin = lncol - .1, ymax = lncol + .1), fill = "darkgray", data_error) +
      geom_rect(aes(xmin = lnrow - .1, xmax = lnrow + .1, ymin = lncol - .1, ymax = lncol + .1, fill = log10(time_method))) +
      scale_fill_distiller(palette = "RdYlBu") +
      scale_x_continuous(breaks = axis_scale$lnrow, labels = axis_scale$nrow) +
      scale_y_continuous(breaks = axis_scale$lnrow, labels = axis_scale$nrow) +
      theme_classic() +
      theme(legend.position = "bottom") +
      labs(x = "# cells", y = "# features", fill = "Log10(Time)") +
      facet_wrap(~ orig_dataset_id, ncol = 1) +
      coord_equal()
    # g2

    tmp_error <- bind_rows(data_error, data_noerror %>% filter(log10(max_mem) < 8))
    g3 <-
      ggplot(data_noerror %>% filter(log10(max_mem) >= 8)) +
      geom_rect(aes(xmin = lnrow - .1, xmax = lnrow + .1, ymin = lncol - .1, ymax = lncol + .1), fill = "darkgray", tmp_error) +
      geom_rect(aes(xmin = lnrow - .1, xmax = lnrow + .1, ymin = lncol - .1, ymax = lncol + .1, fill = log10(max_mem))) +
      scale_fill_distiller(palette = "RdYlBu") +
      scale_x_continuous(breaks = axis_scale$lnrow, labels = axis_scale$nrow) +
      scale_y_continuous(breaks = axis_scale$lnrow, labels = axis_scale$nrow) +
      theme_classic() +
      theme(legend.position = "bottom") +
      labs(x = "# cells", y = "# features", fill = "Log10(Max mem)") +
      facet_wrap(~ orig_dataset_id, ncol = 1) +
      coord_equal()
    # g3

    g4a <-
      ggplot(pred_method) +
      geom_rect(aes(xmin = lnrow - .1, xmax = lnrow + .1, ymin = lncol - .1, ymax = lncol + .1, fill = time_lpred)) +
      scale_fill_distiller(palette = "RdYlBu") +
      scale_x_continuous(breaks = axis_scale$lnrow, labels = axis_scale$nrow) +
      scale_y_continuous(breaks = axis_scale$lnrow, labels = axis_scale$nrow) +
      theme_classic() +
      theme(legend.position = "bottom") +
      labs(x = "# cells", y = "# features", fill = "Predicted time") +
      coord_equal()
    g4b <-
      ggplot(pred_method) +
      geom_rect(aes(xmin = lnrow - .1, xmax = lnrow + .1, ymin = lncol - .1, ymax = lncol + .1, fill = mem_lpred)) +
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
  } else {
    patchwork::wrap_plots(
      g1 + labs(title = "Execution information")
    ) +
      patchwork::plot_annotation(
        title = paste0("Scalability results for ", data_method$method_name[[1]]),
        theme = theme(title = element_text(size = 20))
      )
  }
})

dir.create(result_file("results"), showWarnings = FALSE)
dir.create(result_file("results/error_class_plots"), showWarnings = FALSE)
# pdf(result_file("results.pdf"), width = 16, height = 12)
# walk(seq_along(method_ids), function(i) {
pbapply::pblapply(seq_along(method_ids), cl = 8, function(i) {
  mid <- method_ids[[i]]
  pl <- plots[[i]]
  cat("Plotting ", mid, "\n", sep = "")
  # print(pl)
  ggsave(result_file(c("results/", mid, ".png")), pl, width = 12, height = 14, dpi = 100)
})
# dev.off()



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

error_statuses <- unique(data$error_status)

datasets_info <- data %>% select(dataset_id, orig_dataset_id, lnrow, lncol)

pbapply::pblapply(method_ids, cl = 8, function(mid) {
  selection <-
    data %>%
    filter(dataset_id %in% scaling_avail, method_id == mid, error_status != "no_error") %>%
    mutate(
      error_text = paste0(stdout, "\n", stderr, "\n", error_message)
    )

  sta_lines <- map(error_statuses, function(sta) {
    sel_sta <- selection %>% filter(error_status == sta)

    if (nrow(sel_sta) == 0) {
      return(NULL)
    }

    if (nrow(sel_sta) > 2) {
      clusts <-
        sel_sta$error_text %>%
        map_chr(last_lines, num = 5) %>%
        str_replace_all("Rtmp[^\\n ]*", "") %>%
        str_replace_all("[^a-zA-Z]", "") %>%
        cluster(num_diffs = 10)
    } else if (nrow(sel_sta) == 1) {
      clusts <- 1
    } else {
      clusts <- c()
    }

    cluster_lines <- unlist(lapply(unique(clusts), function(cl) {
      sel_cl <- sel_sta %>% slice(which(clusts == cl))

      g <-
        ggplot(sel_cl) +
        geom_rect(aes(xmin = lnrow - .1, xmax = lnrow + .1, ymin = lncol - .1, ymax = lncol + .1), datasets_info %>% anti_join(sel_cl, by = "dataset_id"), fill = "lightgray") +
        geom_rect(aes(xmin = lnrow - .1, xmax = lnrow + .1, ymin = lncol - .1, ymax = lncol + .1, fill = error_status)) +
        scale_fill_manual(values = dynbenchmark::method_status_colours) +
        scale_x_continuous(breaks = axis_scale$lnrow, labels = axis_scale$nrow) +
        scale_y_continuous(breaks = axis_scale$lnrow, labels = axis_scale$nrow) +
        theme_classic() +
        theme(legend.position = "bottom") +
        labs(x = "# cells", y = "# features", fill = "Status") +
        facet_wrap(~ orig_dataset_id, nrow = 1) +
        coord_equal()

      ggsave(result_file(c("results/error_class_plots/", mid, "_", sta, "_", cl, ".png")), g, width = 10, height = 2.8)

      vals <- sel_cl %>% dynutils::extract_row_to_list(1)
      c(
        "### ERROR CLUSTER ", sta %>% toupper(), " -- ", cl, "\n",
        "![Cluster plot](error_class_plots/", mid, "_", sta, "_", cl, ".png)\n",
        "\n",
        " * Number of instances: ", nrow(sel_cl), "\n",
        " * Dataset ids: ", paste(sel_cl$dataset_id, collapse = ", "), "\n",
        "\n",
        "Last 10 lines of ", sel_cl$dataset_id[[1]], ":\n",
        "```\n",
        last_lines(vals$error_text, num = 10), "\n",
        "```\n",
        "\n"
      )
    }))

    paste0(
      c(
        "## ERROR STATUS ", sta %>% toupper(), "\n",
        "\n",
        cluster_lines
      ),
      collapse = ""
    )
  })


  lines <- paste0(c(
    "# ", mid, "\n",
    "![Overview](", mid, ".png)\n",
    "\n",
    unlist(sta_lines)
  ), collapse = "")

  readr::write_lines(lines, result_file(c("results/", mid, "_overview.md")))

  invisible()
})
