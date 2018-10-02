#' Classify the models and generate scalability figures

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

##########################################################
###############        CLASSIFY TIME       ###############
##########################################################

time_classification_design <- bind_rows(
  tibble(
    feature = "n_cells",
    n_cells = seq(100, 100000, 100),
    n_features = 10000
  ),
  tibble(
    feature = "n_features",
    n_features = seq(100, 100000, 100),
    n_cells = 10000
  )
)

method_predict_time <- models %>% select(method_id, predict_time) %>% deframe()

time_classification_timings <- crossing(
  time_classification_design,
  method_id = names(method_predict_time)
) %>%
  group_by(method_id) %>%
  mutate(time = first(method_predict_time[method_id])(n_cells, n_features)) %>%
  ungroup()


classify_curve <- function(slope, curvature) {
  case_when(
    slope < 0.001 ~ "constant / low slope",
    curvature < -0.05 ~ "sublinear",
    curvature < 0.05 ~ "linear",
    TRUE ~ "superlinear"
  )
}


time_classification <- time_classification_timings %>%
  group_by(method_id, feature) %>%
  summarise(
    slope = mean(diff(time, differences = 1)),
    curvature = mean(seq(0, 1, length.out = n()) - scale_minmax(time)),
    class = classify_curve(slope, curvature)
  ) %>%
  ungroup()

##########################################################
###############  GENERATE SUMMARY FIGURE   ###############
##########################################################

method_order <- models %>% arrange(-time_lpred) %>% pull(method_id)
scale_y_methods <- scale_y_continuous("", breaks = seq_along(method_order), labels = label_method(method_order), expand = c(0, 0))
scale_y_methods_empty <- scale_y_continuous("", breaks = seq_along(method_order) + 0.5, labels = NULL, expand = c(0, 0))
no_margin_sides <- theme(plot.margin = margin(), axis.ticks.y = element_blank(), axis.title.y = element_blank())

time_limits <- c(log10(0.1), log10(60*60*72.01))
time_breaks <- log10(c(1, 60, 60*60, 60*60*24.01))
scale_x_time <- scale_x_continuous(label_long("average_time"), limits = time_limits, breaks = time_breaks, labels = label_time(10^time_breaks), expand = c(0, 00), position = "top")
scale_x_time
even_background <- geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = as.numeric(method_id) - 0.5, ymax = as.numeric(method_id) + 0.5), data = tibble(method_id = factor(method_order, method_order)) %>% filter(as.numeric(method_id) %% 2 == 1), fill = "#DDDDDD")

## Plot average time
plot_average_time <- models %>%
  mutate(
    method_id = factor(method_id, method_order),
    time_lpred = ifelse(time_lpred < -1, -0.8, time_lpred)
  ) %>%
  select(method_id, time_lpred) %>%
  ggplot() +
  even_background +
  geom_rect(aes(xmin = time_limits[[1]], xmax = time_lpred, ymin = as.numeric(method_id) - 0.4, ymax = as.numeric(method_id) + 0.4), stat = "identity", fill = "#333333") +
  geom_text(aes(x = time_lpred, y = as.numeric(method_id), label = paste0(" ", label_time(10^time_lpred))), hjust = 0) +
  scale_y_methods +
  scale_x_time +
  theme_pub() +
  theme(panel.grid.minor.y = element_line(color = "#AAAAAA"), plot.margin = margin())
plot_average_time

## Plot average memory
mem_limits <- log10(c(10^8, 10^10))
mem_breaks <- log10(c(10^8, 10^9, 10^10))
scale_x_mem <- scale_x_continuous(label_long("average_max_memory"), limits = mem_limits, breaks = mem_breaks, labels = label_memory(10^mem_breaks), expand = c(0, 0), position = "top")

plot_average_memory <- models %>%
  mutate(method_id = factor(method_id, method_order)) %>%
  select(method_id, mem_lpred) %>%
  ggplot() +
  even_background +
  geom_rect(aes(xmin = mem_limits[[1]], xmax = mem_lpred, ymin = as.numeric(method_id) - 0.4, ymax = as.numeric(method_id) + 0.4), stat = "identity", fill = "#333333") +
  geom_text(aes(x = mem_lpred, y = as.numeric(method_id), label = paste0(" ", label_memory(10^mem_lpred))), hjust = 0) +
  scale_y_methods +
  scale_x_mem +
  theme_pub() +
  theme(panel.grid.minor.y = element_line(color = "#AAAAAA"), plot.margin = margin())

plot_average_memory

## Plot the classification and small multiples
plotdata_time_classification <- time_classification_timings %>%
  left_join(time_classification, c("method_id", "feature")) %>%
  mutate(method_id = factor(method_id, method_order)) %>%
  mutate(
    ymin = as.numeric(method_id),
    xmin = as.numeric(factor(feature))
  ) %>%
  group_by(
    method_id, feature
  ) %>%
  filter(
    row_number() %% 10 == 0
  ) %>%
  mutate(
    x = xmin + row_number() / n() * 0.95 + 0.025,
    y = ymin + scale_minmax(time) * 0.9 + 0.05
  ) %>%
  ungroup()

plotdata_time_classification_boxes <- plotdata_time_classification %>%
  group_by(
    method_id, feature
  ) %>%
  summarise(
    xmin = first(xmin),
    xmax = xmin + 1,
    ymin = first(ymin),
    ymax = ymin + 1
  ) %>%
  left_join(time_classification, c("method_id", "feature"))

plotdata_time_classification <- plotdata_time_classification %>% select(method_id, feature, x, y)


plot_time_classification <- plotdata_time_classification %>%
  ggplot(aes(group = paste0(method_id, feature))) +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = class), data = plotdata_time_classification_boxes, color = NA) +
  geom_line(aes(x = x, y = y), color = "white", size = 1) +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), data = plotdata_time_classification_boxes, color = "#333333", fill = NA, size = 0.25) +
  scale_y_methods_empty +
  scale_x_continuous("Time complexity" , expand = c(0, 0), breaks = c(1.5, 2.5), labels = c("Cells", "Features"), position = "top") +
  scale_fill_manual("", values = c("constant / low slope" = "#2ECC40", "sublinear" = "#0074D9", "linear" = "#FF851B", "superlinear" = "#FF4136")) +
  theme_pub() +
  theme(legend.position = "bottom") +
  no_margin_sides

plot_time_classification

##
slope_limits <- c(-2, log10(60*60*72.01))
slope_breaks <- log10(c(1, 60, 60*60, 60*60*24.01))
scale_x_slope <- scale_x_continuous(limits = slope_limits, breaks = slope_breaks, labels = label_time(10^slope_breaks), expand = c(0, 0), position = "top")

plot_slope <- function(feature) {
  time_classification %>%
    filter(feature == !!feature) %>%
    mutate(method_id = factor(method_id, method_order)) %>%
    mutate(slope = scales::squish(slope, 10^slope_limits)) %>%
    ggplot() +
    even_background +
    geom_rect(aes(xmin = first(slope_limits), xmax = log10(slope), ymin = as.numeric(method_id) - 0.4, ymax = as.numeric(method_id) + 0.4), stat = "identity", fill = "#333333") +
    geom_text(aes(x = log10(slope), y = as.numeric(method_id), label = paste0(" ", label_time(slope))), hjust = 0) +
    scale_y_methods_empty +
    scale_x_slope +
    labs(x = label_long(paste0("time_per_100_", gsub("n_", "", feature)))) +
    theme_pub() +
    no_margin_sides +
    theme(panel.grid.major.y = element_line(color = "#AAAAAA"))
}

plot_slope_cells <- plot_slope("n_cells")
plot_slope_features <- plot_slope("n_features")
plot_slope_cells


##

g <- patchwork::wrap_plots(
  plot_average_time,
  plot_time_classification,
  plot_slope_cells,
  plot_slope_features,
  plot_average_memory,
  widths = c(2, 1, 2, 2, 2),
  nrow = 1
)

g
ggsave(result_file("ranking.svg"), g, width = 14, height = 12)
write_rds(g, result_file("ranking.rds"))


##########################################################
############### GENERATE INDIVIDUAL FIGURE ###############
##########################################################
scale_x_nrow <- scale_x_continuous(breaks = seq(1, 6), labels = label_thousands(10^seq(1, 6)), expand = c(0,0))
scale_y_ncol <- scale_y_continuous(breaks = seq(1, 6), labels = label_thousands(10^seq(1, 6)), expand = c(0,0))

method_ids <- unique(data$method_id) %>% setdiff("error")

plots <- map(method_ids, function(method_id) {
  print(method_id)
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
    scale_x_nrow + scale_y_ncol +
    theme_classic() +
    theme(legend.position = "bottom") +
    labs(x = "# cells", y = "# features", fill = "Status") +
    facet_wrap(~ orig_dataset_id, ncol = 1) +
    coord_equal()
  # g1

  if (nrow(data_noerror) > 0) {
    # g2
    g2 <-
      ggplot(data_noerror) +
      geom_rect(aes(xmin = lnrow - .1, xmax = lnrow + .1, ymin = lncol - .1, ymax = lncol + .1), fill = "darkgray", data_error) +
      geom_rect(aes(xmin = lnrow - .1, xmax = lnrow + .1, ymin = lncol - .1, ymax = lncol + .1, fill = log10(time_method))) +
      scale_fill_distiller(palette = "RdYlBu") +
      scale_x_nrow + scale_y_ncol +
      theme_classic() +
      theme(legend.position = "bottom") +
      labs(x = "# cells", y = "# features", fill = "Log10(Time)") +
      facet_wrap(~ orig_dataset_id, ncol = 1) +
      coord_equal()

    tmp_error <- bind_rows(data_error, data_noerror %>% filter(log10(max_mem) < 8))
    g3 <-
      ggplot(data_noerror %>% filter(log10(max_mem) >= 8)) +
      geom_rect(aes(xmin = lnrow - .1, xmax = lnrow + .1, ymin = lncol - .1, ymax = lncol + .1), fill = "darkgray", tmp_error) +
      geom_rect(aes(xmin = lnrow - .1, xmax = lnrow + .1, ymin = lncol - .1, ymax = lncol + .1, fill = log10(max_mem))) +
      scale_fill_distiller(palette = "RdYlBu") +
      scale_x_nrow + scale_y_ncol +
      theme_classic() +
      theme(legend.position = "bottom") +
      labs(x = "# cells", y = "# features", fill = "Log10(Max mem)") +
      facet_wrap(~ orig_dataset_id, ncol = 1) +
      coord_equal()
    # g3

    g4a <-
      ggplot(pred_method %>% select(lnrow, lncol, time_lpred)) +
      geom_rect(aes(xmin = lnrow - .1, xmax = lnrow + .1, ymin = lncol - .1, ymax = lncol + .1, fill = time_lpred)) +
      scale_fill_distiller(palette = "RdYlBu") +
      scale_x_nrow + scale_y_ncol +
      theme_classic() +
      theme(legend.position = "bottom") +
      labs(x = "# cells", y = "# features", fill = "Predicted time") +
      coord_equal()

    g4b <-
      ggplot(pred_method) +
      geom_rect(aes(xmin = lnrow - .1, xmax = lnrow + .1, ymin = lncol - .1, ymax = lncol + .1, fill = mem_lpred)) +
      scale_fill_distiller(palette = "RdYlBu") +
      scale_x_nrow + scale_y_ncol +
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
map(seq_along(method_ids), function(i) {
  mid <- method_ids[[i]]
  pl <- plots[[i]]
  cat("Plotting ", mid, "\n", sep = "")
  # print(pl)
  ggsave(result_file(c("results/", mid, ".png")), pl, width = 12, height = 14, dpi = 100)
})
# dev.off()


##########################################################
########### GENERATE INDIVIDUAL EXAMPLE FIGURE ###########
##########################################################

method_ids <- c("mst", "paga", "slingshot", "elpigraph", "monocle_ddrtree", "ouija")

scale_fill_time <- scale_fill_viridis_c("", option = "inferno", labels = label_time, trans = "log", breaks = function(range) {c(first(range), 10^mean(log10(range)), last(range))}, limits = c(1, 60*60*72), oob = scales::squish)

plots <- list()
for (method_id in method_ids) {
  data_method <- data %>% filter(method_id == !!method_id)
  model_method <- models %>% filter(method_id == !!method_id)
  data_noerror <- data_method %>% filter(error_status == "no_error")
  data_error <- data_method %>% filter(error_status != "no_error")
  pred_method <- data_pred %>% filter(method_id == !!method_id)

  g2b <- ggplot(data_noerror, aes(xmin = lnrow - .1, xmax = lnrow + .1, ymin = lncol - .1, ymax = lncol + .1)) +
    geom_rect(fill = "darkgray", data = data_error) +
    geom_rect(aes(fill = time_method)) +
    scale_fill_time + scale_x_nrow + scale_y_ncol +
    labs(x = "# cells", y = "# features") +
    coord_equal() +
    ggtitle(glue::glue("{label_method(method_id)}\nObserved time")) +
    theme_pub() +
    theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(size = 12))
  plots <- c(plots, list(g2b))
  g4a <-
    ggplot(pred_method %>% select(lnrow, lncol, time_lpred)) +
    geom_rect(aes(xmin = lnrow - .1, xmax = lnrow + .1, ymin = lncol - .1, ymax = lncol + .1, fill = 10^time_lpred)) +
    scale_x_nrow + scale_y_ncol + scale_fill_time +
    labs(x = "# cells", y = "# features") +
    coord_equal() +
    ggtitle(glue::glue("Predicted time")) +
    theme_pub() +
    theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(size = 12))
  plots <- c(plots, list(g4a))
}

plot_example <- plots %>%
  map_at(2:9999, ~. + theme(axis.title = element_blank())) %>%
  map_at(1, ~. + theme(legend.position = "left")) %>%
  patchwork::wrap_plots(nrow = 2, byrow = FALSE)
plot_example
write_rds(plot_example, derived_file("example.rds"))

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
        scale_x_nrow + scale_y_ncol +
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


##########################################################
###############   GENERATE SUPP FIGURE     ###############
##########################################################

plot_scaling <- patchwork::wrap_plots(
  read_rds(derived_file("example.rds")) %>% patchwork::wrap_elements(),
  read_rds(result_file("ranking.rds")) %>% patchwork::wrap_elements(),
  heights = c(2, 5),
  ncol = 1
) +
  patchwork::plot_annotation(tag_levels = "a")

ggsave(result_file("scaling.pdf"), width = 14, height = 16)

