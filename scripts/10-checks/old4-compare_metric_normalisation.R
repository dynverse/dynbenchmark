library(dynbenchmark)
library(tidyverse)
library(dynplot)

experiment("06-optimise_parameters/4-compare_metric_normalisation")

outputs_ind <- read_rds(derived_file("outputs_postprocessed.rds", "06-optimise_parameters/3-evaluate_parameters"))$outputs_ind %>%
  mutate(rf_mse_neg = 1 - rf_mse)

metrics <- c("correlation", "edge_flip", "rf_mse_neg")

dataset_cols <-
  outputs_ind %>%
  select(trajectory_type, dataset_id) %>%
  unique() %>%
  group_by(trajectory_type) %>%
  mutate(dataset_colour = percent_rank(seq_len(n()))) %>%
  ungroup() %>%
  mutate(dataset_colour = factor(rank(dataset_colour))) %>%
  select(-trajectory_type)



scaletanh_trafo <- function (x, remove_errored = FALSE, determine_coeff = TRUE) {
  xn <- x
  if (remove_errored) xn <- xn[xn != 0]
  y <- (x - mean(xn)) / var(xn)

  if (determine_coeff) {
    yn <- (xn - mean(xn)) / var(xn)
    coeff <- 3 / max(abs(yn))
  } else {
    coeff <- .01
  }

  tanh(y * coeff) / 2 + .5
}

scalesigmoid_trafo <- function (x, remove_errored = FALSE, determine_coeff = TRUE) {
  xn <- x
  if (remove_errored) xn <- xn[xn != 0]
  y <- (x - mean(xn)) / var(xn)
  if (determine_coeff) {
    yn <- (xn - mean(xn)) / var(xn)
    coeff <- 5 / max(abs(yn))
  } else {
    coeff <- 4
  }
  sigmoid::sigmoid(y * coeff)
}

medquansigmoid_trafo <- function (x, remove_errored = FALSE, determine_coeff = TRUE) {
  xn <- x
  if (remove_errored) xn <- xn[xn != 0]
  y <- (x - median(xn)) / diff(quantile(xn, c(.05, .95)))
  if (determine_coeff) {
    yn <- (xn - median(xn)) / diff(quantile(xn, c(.05, .95)))
    coeff <- 5 / max(abs(yn))
  } else {
    coeff <- 4
  }
  sigmoid::sigmoid(y * coeff)
}


medmadsigmoid_trafo <- function (x, remove_errored = FALSE, determine_coeff = TRUE) {
  xn <- x
  if (remove_errored) xn <- xn[xn != 0]
  y <- (x - median(xn)) / mad(xn)
  if (determine_coeff) {
    yn <- (xn - median(xn)) / mad(xn)
    coeff <- 5 / max(abs(yn))
  } else {
    coeff <- 5
  }
  sigmoid::sigmoid(y * 4)
}

minmax_trafo <- function (x, invert = FALSE, remove_errored = FALSE) {
  xn <- x
  if (remove_errored) xn <- xn[xn != 0]
  y <- (x - min(xn)) / (max(xn) - min(xn))
  y[y < 0] <- 0
  if (invert) y <- 1 - y
  y
}

group_by_dataset_id <- function(dataset_id, score, fun, ...) {
  data_frame(dataset_id, score) %>%
    group_by(dataset_id) %>%
    mutate(norm_score = fun(score, ...)) %>%
    ungroup() %>%
    pull(norm_score)
}

quantile_normalise <- function(dataset_id, score, repeats = 100) {
  rowMeans(sapply(seq_len(repeats), function(rep) {
    data_frame(dataset_id, score) %>%
      group_by(dataset_id) %>%
      mutate(rank_score = rank(score, ties.method = "random")) %>%
      ungroup() %>%
      group_by(rank_score) %>%
      mutate(quantile_score = mean(score)) %>%
      ungroup() %>%
      pull(quantile_score)
  }))
}

is_min <- function(a, b) {
  z <- abs(a - b)
  z == min(z)
}



score_funs <- list(
  identity = function(dataset_id, score) score,
  percentrank = function(dataset_id, score) group_by_dataset_id(dataset_id, score, percent_rank),
  quantile = quantile_normalise,
  scale_tanh = function(dataset_id, score) group_by_dataset_id(dataset_id, score, scaletanh_trafo, remove_errored = T, determine_coeff = F),
  scale_tanh_sc = function(dataset_id, score) group_by_dataset_id(dataset_id, score, scaletanh_trafo, remove_errored = T, determine_coeff = T),
  scale_sigmoid = function(dataset_id, score) group_by_dataset_id(dataset_id, score, scalesigmoid_trafo, remove_errored = T, determine_coeff = F),
  scale_sigmoid_sc = function(dataset_id, score) group_by_dataset_id(dataset_id, score, scalesigmoid_trafo, remove_errored = T, determine_coeff = T),
  medquan_sigmoid = function(dataset_id, score) group_by_dataset_id(dataset_id, score, medquansigmoid_trafo, remove_errored = T, determine_coeff = F),
  medquan_sigmoid_sc = function(dataset_id, score) group_by_dataset_id(dataset_id, score, medquansigmoid_trafo, remove_errored = T, determine_coeff = T),
  # medmad_sigmoid = function(dataset_id, score) group_by_dataset_id(dataset_id, score, medmadsigmoid_trafo, remove_errored = T, determine_coeff = F),
  # medmad_sigmoid_sc = function(dataset_id, score) group_by_dataset_id(dataset_id, score, medmadsigmoid_trafo, remove_errored = T, determine_coeff = T),
  minmax_rem = function(dataset_id, score) group_by_dataset_id(dataset_id, score, minmax_trafo, remove_errored = TRUE)

)

out_scalings <-
  lapply(seq_along(score_funs), function(mi) {
    score_fun <- score_funs[[mi]]
    score_name <- names(score_funs)[[mi]]
    cat("Processing ", mi, " ", score_name, "\n", sep = "")

    oi <- outputs_ind %>%
      select(dataset_id, method_short_name, trajectory_type, one_of(metrics)) %>%
      gather(metric, score, one_of(metrics)) %>%
      group_by(metric) %>%
      mutate(
        norm_score = score_fun(dataset_id, score),
        is_med = is_min(median(score), score),
        is_q05 = is_min(quantile(score, .05), score),
        is_q95 = is_min(quantile(score, .95), score)
      ) %>%
      ungroup() %>%
      left_join(dataset_cols, by = "dataset_id")

    ois <- oi %>%
      group_by(dataset_id, metric) %>%
      summarise(mean__raw = mean(score), mean__norm = mean(norm_score), q25__raw = quantile(score, .25), q25__norm = quantile(norm_score, .25), q75__raw = quantile(score, .75), q75__norm = quantile(norm_score, .75)) %>%
      ungroup() %>%
      gather(xxx, value, -dataset_id, -metric) %>%
      mutate(stage = gsub("__.*", "", xxx), type = gsub(".*__", "", xxx))

    ######## RIGHT
    zzz <- oi %>%
      group_by(metric) %>%
      filter(score != 0) %>%
      mutate(score_sc = scale_minmax(score), norm_score_sc = scale_minmax(norm_score)) %>%
      ungroup()

    quan <- zzz %>%
      group_by(metric) %>%
      do({
        data_frame(
          metric = unique(.$metric),
          xxx = seq(0, 1, length.out = 101),
          quan_score = quantile(.$score_sc, xxx),
          quan_norm_score = quantile(.$norm_score_sc, xxx)
        )
      }) %>%
      ungroup()

    lab <- quan %>%
      group_by(metric) %>%
      summarise(cor = cor(quan_score, quan_norm_score)) %>%
      mutate(x = .25, y = .75, label = paste0("Cor = ", round(cor, 3)))

    gqq <- ggplot(quan) +
      geom_point(aes(quan_score, quan_norm_score)) +
      geom_label(aes(x, y, label = label), lab) +
      theme_bw() +
      facet_wrap(~metric, nrow = 1) +
      labs(x = "Raw", y = "Normalised")

    vline <- data_frame(
      x = c(.25, .5, .75),
      stage = c("q25", "mean", "q75")
    )

    driftdf <-
      ois %>%
      filter(type == "norm") %>%
      left_join(vline, by = "stage") %>%
      group_by(metric, stage, type, xxx) %>%
      summarise(drift = mean(abs(value - x))) %>%
      ungroup() %>%
      mutate(x = .25, y = .75)

    gden <- ggplot(ois) +
      geom_vline(aes(xintercept = x), colour = "black", vline) +
      geom_density(aes(x = value, y = ..scaled.., colour = type)) +
      geom_label(aes(x, y, label = paste0("Mean diff = ", round(drift, 2))), driftdf) +
      facet_grid(stage~metric) +
      theme_bw()

    gright <- cowplot::plot_grid(gden, gqq, ncol = 1, rel_heights = c(2, 1))

    ######### LEFT
    g <- ggplot(oi, aes(score, norm_score, colour = dataset_colour)) +
      geom_line() +
      geom_point(aes(shape = "q50"), oi %>% filter(is_med)) +
      geom_point(aes(shape = "q05"), oi %>% filter(is_q05)) +
      geom_point(aes(shape = "q95"), oi %>% filter(is_q95)) +
      theme_bw() +
      theme(legend.position = "none") +
      facet_grid(metric~trajectory_type, scales = "free") +
      labs(title = score_name)

    g2 <- ggplot(oi) +
      geom_density(aes(x = norm_score, y = ..scaled.., group = dataset_id, colour = dataset_colour)) +
      facet_grid(metric~trajectory_type, scales = "free") +
      theme_bw() +
      theme(legend.position = "none")

    gleft <- cowplot::plot_grid(g, g2, ncol = 1)

    plot_out <- cowplot::plot_grid(gleft, gright, ncol = 2, rel_widths = c(2,1))

    stats <-
      lab %>% select(metric, cor) %>%
      left_join(driftdf %>% select(metric, stage, drift) %>% spread(stage, drift), by = "metric") %>%
      left_join(driftdf %>% select(metric, stage, drift) %>% group_by(metric) %>% summarise(mean_drift = mean(drift)), by = "metric")
    stats <- bind_rows(stats, stats %>% summarise_if(is.numeric, mean) %>% mutate(metric = "mean")) %>%
      mutate(score_name = score_name)

    lst(plot_out, stats)
  })

pdf(figure_file("compare_scaling.pdf"), 30, 16)
for (o in out_scalings) {
  print(o$plot_out)
}
dev.off()

stats <- out_scalings %>% map_df(~ .$stats)

zz <- ggplot(stats, aes(mean_drift, cor)) +
  geom_point() +
  ggrepel::geom_text_repel(aes(label = score_name), nudge_y = .02) +
  facet_wrap(~metric, nrow = 1) +
  theme_bw()
ggsave(figure_file("summ.pdf"), zz, width = 16, height = 6)
