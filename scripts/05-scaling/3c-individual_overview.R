##########################################################
############### GENERATE INDIVIDUAL FIGURE ###############
##########################################################
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
map(seq_along(method_ids), function(i) {
  mid <- method_ids[[i]]
  pl <- plots[[i]]
  cat("Plotting ", mid, "\n", sep = "")
  # print(pl)
  ggsave(result_file(c("results/", mid, ".png")), pl, width = 12, height = 14, dpi = 100)
})
