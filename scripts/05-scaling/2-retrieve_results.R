library(dynbenchmark)
library(tidyverse)
library(dynutils)

experiment("05-scaling")

##########################################################
###############      RETRIEVE RESULTS      ###############
##########################################################

# fetch results from cluster
benchmark_fetch_results()

# bind results in one data frame (without models)
design <- read_rds(derived_file("design.rds"))
execution_output <- benchmark_bind_results(load_models = TRUE)
methods_info <- design$methods
datasets_info <- design$datasets

##########################################################
###############         FIT MODELS         ###############
##########################################################

data <-
  execution_output %>%
  select(method_id, dataset_id, errored = dummy, error_status, starts_with("time_")) %>%
  left_join(datasets_info %>% select(dataset_id = id, lnrow, lncol, lsum, nrow, ncol, memory), by = "dataset_id") %>%
  left_join(methods_info %>% select(method_id = id, method_name = name), by = "method_id")

axis_scale <- data %>% select(lnrow, nrow) %>% unique() %>% filter(lnrow %% 1 == 0)

models <-
  data %>%
  filter(error_status == "no_error") %>%
  as_tibble() %>%
  group_by(method_id) %>%
  filter(n() > 10) %>% # need at least a few data points
  do({
    dat <- .
    out <- dat %>% select(method_id, method_name) %>% unique()
    model <- glm(log10(time_method) ~ lnrow + lncol, data = dat)
    # model <- glm(log10(time_method) ~ lnrow + lncol + lnrow * lncol, data = dat)
    sum <- summary(model)

    coef_names <- c("(Intercept)" = "intercept", "lnrow" = "lnrow", "lncol" = "lncol", "lnrow:lncol" = "lnrow_lncol")[rownames(sum$coefficients)]
    coef_values <- set_names(
      c(sum$coefficients[,1], sum$coefficients[,4]),
      c(coef_names, paste0("p_", coef_names))
    )

    lpredtime <- predict(model, datasets_info) %>% sum()

    out %>%
      mutate(model = list(model)) %>%
      bind_cols(as.data.frame(t(coef_values))) %>%
      mutate(lpredtime)
  }) %>%
  ungroup()

data_pred <- mapdf_dfr(models, function(model) {
  dat <- datasets_info %>% select(dataset_id = id, lnrow, lncol)
  dat$method_id <- model$method_id
  dat$method_name <- model$method_name
  dat$lpredtime <- predict(model$model, dat)
  dat
})

method_ids <- unique(data$method_id) %>% setdiff("error")

##########################################################
###############         SAVE DATA          ###############
##########################################################
write_rds(lst(data, data_pred, models), result_file("scaling.rds"), compress = "xz")


##########################################################
###############       CREATE FIGURES       ###############
##########################################################
columns <-
  data_frame(
    id = c("lpredtime", "baseline", "# genes", "# features"),
    x = c(1.1, 2.2, 3.3, 4.4) + .5
  )
g <- models %>%
  # mutate_at(vars(starts_with("p_")), function(x) log10(x)) %>%
  mutate_at(c("lpredtime", "lnrow", "lncol", "intercept"), function(x) dynutils::scale_minmax(x)) %>%
  arrange(lpredtime) %>%
  mutate(
    method_id_f = factor(method_id, levels = method_id),
    method_name_f = factor(method_name, levels = method_name),
    y = -as.integer(method_id_f)
  ) %>%
  ggplot() +
  geom_text(aes(1, y, label = method_name), hjust = 1) +
  geom_text(aes(x, 0, label = id), columns) +
  geom_rect(aes(ymin = y - .45, ymax = y + .45, xmin = 1.1, xmax = 1.1 + lpredtime)) +
  geom_rect(aes(ymin = y - .45, ymax = y + .45, xmin = 2.2, xmax = 2.2 + lnrow, fill = p_lnrow)) +
  geom_rect(aes(ymin = y - .45, ymax = y + .45, xmin = 3.3, xmax = 3.3 + lncol, fill = p_lncol)) +
  geom_rect(aes(ymin = y - .45, ymax = y + .45, xmin = 4.4, xmax = 4.4 + intercept, fill = p_intercept)) +
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

ggsave(figure_file("ranking.svg"), g, width = 10, height = 10)


g2 <-
  ggplot(models, aes(lnrow, lncol)) +
  geom_point(aes(size = intercept, colour = intercept)) +
  ggrepel::geom_text_repel(aes(label = method_id), size = 3) +
  theme_classic() +
  scale_colour_distiller(palette = "RdYlBu") +
  theme(legend.position = "bottom") +
  labs(x = "# cells", y = "# features", colour = "baseline", size = "baseline")

g3 <-
  ggplot(models, aes(lnrow, -log10(p_lnrow))) +
  geom_point(aes(colour = lpredtime, size = lpredtime)) +
  ggrepel::geom_text_repel(aes(label = method_id), size = 3) +
  theme_classic() +
  scale_colour_distiller(palette = "RdYlBu") +
  theme(legend.position = "bottom") +
  labs(x = "# cells", y = "-log10(p_cells)")

g4 <-
  ggplot(models, aes(lncol, -log10(p_lncol))) +
  geom_point(aes(colour = lpredtime, size = lpredtime)) +
  ggrepel::geom_text_repel(aes(label = method_id), size = 3) +
  theme_classic() +
  scale_colour_distiller(palette = "RdYlBu") +
  theme(legend.position = "bottom") +
  labs(x = "# features", y = "-log10(p_features)")

g5 <-
  ggplot(models, aes(intercept, -log10(p_intercept))) +
  geom_point(aes(colour = lpredtime, size = lpredtime)) +
  ggrepel::geom_text_repel(aes(label = method_id), size = 3) +
  theme_classic() +
  scale_colour_distiller(palette = "RdYlBu") +
  theme(legend.position = "bottom") +
  labs(x = "baseline", y = "-log10(p_baseline)")

g <- patchwork::wrap_plots(
  g2,
  g3,
  g4,
  g5,
  nrow = 2
)
ggsave(derived_file("overview.pdf"), g, width = 12, height = 12)






plots <- map(method_ids, function(method_id) {
  methods_info_method <- methods_info %>% filter(id == !!method_id) %>% extract_row_to_list(1)
  data_method <- data %>% filter(method_id == !!method_id)
  model_method <- models %>% filter(method_id == !!method_id)
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
    labs(x = "# cells", y = "# features", fill = "Status")

  if (nrow(model_method) > 0) {
    g2 <-
      ggplot(timings_method) +
      geom_tile(aes(lnrow, lncol), fill = "darkgray", data_method) +
      geom_tile(aes(lnrow, lncol, fill = log10(time_method))) +
      scale_fill_distiller(palette = "RdYlBu") +
      scale_x_continuous(breaks = axis_scale$lnrow, labels = axis_scale$nrow) +
      scale_y_continuous(breaks = axis_scale$lnrow, labels = axis_scale$nrow) +
      theme_classic() +
      theme(legend.position = "bottom") +
      labs(x = "# cells", y = "# features", fill = "Log10(Time)")

    g2p <-
      ggplot(pred_method) +
      geom_tile(aes(lnrow, lncol, fill = lpredtime)) +
      scale_fill_distiller(palette = "RdYlBu") +
      scale_x_continuous(breaks = axis_scale$lnrow, labels = axis_scale$nrow) +
      scale_y_continuous(breaks = axis_scale$lnrow, labels = axis_scale$nrow) +
      theme_classic() +
      theme(legend.position = "bottom") +
      labs(x = "# cells", y = "# features", fill = "Log10(Pred. Time)")

    g3 <-
      ggplot(timings_method, aes(lnrow, time_method, group = lncol, colour = lncol)) +
      geom_point() +
      geom_line() +
      scale_x_continuous(breaks = axis_scale$lnrow, labels = axis_scale$nrow) +
      scale_y_log10(breaks = 10^seq(-5, 5)) +
      scale_colour_distiller(palette = "RdYlBu") +
      theme_classic() +
      theme(legend.position = "bottom") +
      labs(x = "# cells", y = "Log10(Time)", colour = "# features")

    g4 <-
      ggplot(timings_method, aes(lncol, time_method, group = lnrow, colour = lnrow)) +
      geom_point() +
      geom_line() +
      scale_x_continuous(breaks = axis_scale$lnrow, labels = axis_scale$nrow) +
      scale_y_log10(breaks = 10^seq(-5, 5)) +
      scale_colour_distiller(palette = "RdYlBu") +
      theme_classic() +
      theme(legend.position = "bottom") +
      labs(x = "# features", y = "Log10(Time)", colour = "# cells")
  } else {
    g2 <- patchwork::plot_spacer()
    g2p <- patchwork::plot_spacer()
    g3 <- patchwork::plot_spacer()
    g4 <- patchwork::plot_spacer()
  }

  g5 <-
    ggplot(models, aes(lnrow, lncol)) +
    geom_hline(aes(yintercept = lncol), model_method, colour = "darkgray") +
    geom_vline(aes(xintercept = lnrow), model_method, colour = "darkgray") +
    geom_point(aes(size = intercept, colour = intercept)) +
    ggrepel::geom_text_repel(aes(label = method_id), size = 3) +
    theme_classic() +
    scale_colour_distiller(palette = "RdYlBu") +
    theme(legend.position = "bottom") +
    labs(x = "# cells", y = "# features")

  patchwork::wrap_plots(
    g1 + labs(title = "Execution information"),
    g2 + labs(title = "Timings grid"),
    g2p + labs(title = "Predicted timings"),
    g3 + labs(title = "Time versus # cells"),
    g4 + labs(title = "Time versus # genes"),
    g5 + labs(title = "Model coefficients"),
    nrow = 2
  ) +
    patchwork::plot_annotation(
      title = paste0("Scalability results for ", methods_info_method$name),
      theme = theme(title = element_text(size = 20))
    )
})

pdf(derived_file("results.pdf"), width = 15, height = 12)
for (p in plots) {
print(p)
}
dev.off()


#' @examples
#' # examine some errors
#' execution_output %>% filter(method_id == "mpath", error_status == "method_error") %>% mutate(txt = paste0(stdout, stderr, error_message)) %>% pull(txt) %>% head(5) %>% cat
#' execution_output %>% filter(method_id == "dpt", error_status == "method_error") %>% mutate(txt = paste0(stdout, stderr, error_message)) %>% pull(txt) %>% head(5) %>% cat
