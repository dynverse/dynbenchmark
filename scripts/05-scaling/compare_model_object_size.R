manymeth <- map_df(c("monocle_ddrtree", "paga", "slingshot", "scorpius", "grandprix", "recat"), function(method_id) {
  row <- scaling_models %>% filter(method_id == !!method_id)
  model_fun <- row$scaling_models_predict_time[[1]]

  test_spacing <- seq(1, 9, by = .01)
  test_grid <- crossing(
    log_cells = test_spacing,
    log_features = test_spacing
  ) %>%
    mutate(
      n_cells = 10^log_cells,
      n_features = 10^log_features,
      pred_time = model_fun(n_cells, n_features),
      log_pred_time = log10(pred_time)
    )

  train_spacing <- seq(1, 9, by = .1)
  train_grid <- crossing(
    log_cells = train_spacing,
    log_features = train_spacing
  ) %>%
    mutate(
      n_cells = 10^log_cells,
      n_features = 10^log_features,
      pred_time = model_fun(n_cells, n_features),
      log_pred_time = log10(pred_time)
    )
  train_mat <- matrix(train_grid$log_pred_time, nrow = length(train_spacing), ncol = length(train_spacing))

  test_grid$interp <- akima::bicubic(
    x = train_spacing,
    y = train_spacing,
    z = t(train_mat),
    x0 = test_grid$log_cells,
    y0 = test_grid$log_features
  )$z

  # test_grid %>%
  #   mutate(sqerror = (interp - log_pred_time) ^ 2) %>%
  #   summarise(mse = mean(sqerror))
  #
  # ggplot(test_grid) +
  #   geom_raster(aes(log_cells, log_features, fill = interp - log_pred_time)) +
  #   geom_hline(aes(yintercept = train_spacing), data_frame(train_spacing), alpha = .1) +
  #   geom_vline(aes(xintercept = train_spacing), data_frame(train_spacing), alpha = .1) +
  #   scale_fill_distiller(palette = "RdBu", limits = c(-.1, .1)) +
  #   theme_bw()

  resolutions <- seq(.1, 1, length.out = 100)

  manyres <- map_df(resolutions, function(res) {
    train_spacing <- seq(1, 9, by = res)
    train_grid <- crossing(
      log_cells = train_spacing,
      log_features = train_spacing
    ) %>%
      mutate(
        n_cells = 10^log_cells,
        n_features = 10^log_features,
        pred_time = model_fun(n_cells, n_features),
        log_pred_time = log10(pred_time)
      )
    train_mat <- matrix(train_grid$log_pred_time, nrow = length(train_spacing), ncol = length(train_spacing))

    test_grid$interp <- akima::bicubic(
      x = train_spacing,
      y = train_spacing,
      z = t(train_mat),
      x0 = test_grid$log_cells,
      y0 = test_grid$log_features
    )$z

    test_grid %>%
      mutate(sqerror = (interp - log_pred_time) ^ 2) %>%
      summarise(mse = mean(sqerror)) %>%
      mutate(resolution = res, object_size = as.numeric(pryr::object_size(train_mat)), method_id)
  })
})

patchwork::wrap_plots(
  ggplot(manyres) + geom_line(aes(resolution, mse)) + theme_bw(),
  ggplot(manyres) + geom_line(aes(resolution, object_size)) + theme_bw(),
  ncol = 1
)

patchwork::wrap_plots(
  ggplot(manymeth) + geom_point(aes(resolution, mse, colour = method_id)) + theme_bw(),
  ggplot(manymeth) + geom_point(aes(resolution, object_size, colour = method_id)) + theme_bw(),
  ggplot(manymeth) + geom_point(aes(resolution, log10(mse * object_size), colour = method_id)) + theme_bw(),
  ncol = 1
)
