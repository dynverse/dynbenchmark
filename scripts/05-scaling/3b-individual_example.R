##########################################################
########### GENERATE INDIVIDUAL EXAMPLE FIGURE ###########
##########################################################

method_ids_sel <- c("mst", "paga", "slingshot", "elpigraph", "monocle_ddrtree", "ouija")

scale_fill_time <- scale_fill_viridis_c("", option = "inferno", labels = label_time, trans = "log", breaks = function(range) {c(first(range), 10^mean(log10(range)), last(range))}, limits = c(1, 60*60*72), oob = scales::squish)

plots <- list()
for (method_id in method_ids_sel) {
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
