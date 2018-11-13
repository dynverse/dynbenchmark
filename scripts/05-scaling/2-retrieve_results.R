#' Retrieve the results and generate the scalability models

library(dynbenchmark)
library(tidyverse)
library(dynutils)

experiment("05-scaling")

###################################################
###                    FETCH                    ###
###################################################

# If you are the one who submitted the jobs, run:
# benchmark_fetch_results(TRUE)
# qsub::rsync_remote(
#   remote_src = FALSE,
#   path_src = derived_file(remote = FALSE, experiment = "05-scaling"),
#   remote_dest = TRUE,
#   path_dest = derived_file(remote = TRUE, experiment = "05-scaling"),
#   verbose = TRUE,
#   exclude = "*/r2gridengine/*"
# )

# # If you want to download the output from prism
# qsub::rsync_remote(
#   remote_src = TRUE,
#   path_src = derived_file("", remote = TRUE, experiment = "05-scaling"),
#   remote_dest = FALSE,
#   path_dest = derived_file("", remote = FALSE, experiment = "05-scaling"),
#   verbose = TRUE,
#   exclude = "*/r2gridengine/*"
# )

# bind results in one data frame (without models)
execution_output <- benchmark_bind_results(load_models = FALSE)
design <- read_rds(derived_file("design.rds"))
methods_info <- design$methods
datasets_info <- design$datasets %>% filter(id %in% execution_output$dataset_id)

##########################################################
###############         FIT MODELS         ###############
##########################################################

data <-
  execution_output %>%
  select(method_id, dataset_id, errored = dummy, error_status, starts_with("time_"), mem_io:max_mem_gb, stderr, stdout, error_message) %>%
  left_join(datasets_info %>% select(dataset_id = id, orig_dataset_id, lnrow, lncol, lsum, nrow, ncol, memory), by = "dataset_id") %>%
  left_join(methods_info %>% select(method_id = id, method_name = name), by = "method_id")

#' @examples
#' dat <- data %>% filter(method_id == "scorpius")

method_ids <-
  data %>%
  as_tibble() %>%
  group_by(method_id) %>%
  filter(sum(error_status == "no_error") > 10) %>%
  summarise(n = n()) %>%
  pull(method_id)

models <-
  qsub::qsub_lapply(
    X = method_ids,
    qsub_environment = "data",
    qsub_config = qsub::override_qsub_config(memory = "9G", compress = "none"),
    qsub_packages = c("tidyverse", "akima", "carrier", "scam", "strip"),
    FUN = function(method_id) {
      dat <-
        data %>%
        as_tibble() %>%
        filter(method_id == !!method_id, error_status == "no_error") %>%
        mutate(
          time = time_method,
          ltime = log10(time_method),
          mem = max_mem,
          lmem = log10(mem)
        )

      # create a model reduction grid
      model_resolution <- .5
      model_seq <- seq(.5, 9, by = model_resolution)
      model_grid <-
        crossing(
          lnrow = model_seq,
          lncol = model_seq
        )

      # train time model
      dat_time <- dat %>% filter(is.finite(ltime))
      model_time <- scam::scam(ltime ~ s(lnrow, lncol, bs = "tedmi"), data = dat_time) %>%
        strip::strip(keep = "predict")

      # train memory model
      dat_mem <- dat %>% filter(is.finite(lmem), lmem >= 8)
      model_mem <- scam::scam(lmem ~ s(lnrow, lncol, bs = "tedmi"), data = dat_mem) %>%
        strip::strip(keep = "predict")

      # create predict functions
      model_grid$time_lpred <- stats::predict(model_time, model_grid)
      model_grid$mem_lpred <- stats::predict(model_mem, model_grid)

      pred_ltime_mat <- t(matrix(model_grid$time_lpred, nrow = length(model_seq), ncol = length(model_seq)))
      pred_lmem_mat <- t(matrix(model_grid$mem_lpred, nrow = length(model_seq), ncol = length(model_seq)))

      predict_time <- carrier::crate(function(n_cells, n_features) {
        requireNamespace("akima")
        10^akima::bicubic(x = model_seq, y = model_seq, z = pred_mat, x0 = log10(n_cells), y0 = log10(n_features))$z
      }, pred_mat = pred_ltime_mat, model_seq)
      predict_mem <- carrier::crate(function(n_cells, n_features) {
        requireNamespace("akima")
        10^akima::bicubic(x = model_seq, y = model_seq, z = pred_mat, x0 = log10(n_cells), y0 = log10(n_features))$z
      }, pred_mat = pred_lmem_mat, model_seq)

      # calculate preds
      pred_ind <-
        model_grid %>%
        mutate(
          method_id = dat$method_id[[1]],
          nrow = 10^lnrow,
          ncol = 10^lncol,
          lsum = lnrow + lncol,
          time_pred = 10^time_lpred,
          mem_pred = 10^mem_lpred
        ) %>%
        filter(4 - 1e-10 <= lsum, lsum <= 7 + 1e-10)

      # format output
      data %>%
        filter(method_id == !!method_id) %>%
        group_by(method_id) %>%
        summarise(pct_errored = mean(error_status != "no_error")) %>%
        mutate(
          predict_time = list(predict_time),
          predict_mem = list(predict_mem),
          pred_ind = list(pred_ind)
        ) %>%
        mutate(
          time_lpred = mean(log10(pred_ind[[1]]$time_pred)),
          mem_lpred = mean(log10(pred_ind[[1]]$mem_pred))
        )
    }
  ) %>% bind_rows()

data_pred <- bind_rows(models$pred_ind)
models <- models %>% select(-pred_ind)

##########################################################
###############         SAVE DATA          ###############
##########################################################
data$stdout <- headtail(data$stdout, 50)

write_rds(lst(data, data_pred, models), result_file("scaling.rds"), compress = "xz")

##########################################################
###############   MAKE A FEW PREDICTIONS   ###############
##########################################################

list2env(read_rds(result_file("scaling.rds")), environment())

scaling_exp <-
  tribble(
    ~ labnrow, ~ labncol, ~ lnrow, ~ lncol,
    "cells100", "features1m", 2, 6,
    "cells1k", "features100k", 3, 5,
    "cells10k", "features10k", 4, 4,
    "cells100k", "features1k", 5, 3,
    "cells1m", "features100", 6, 2
  ) %>%
  mutate(
    n_cells = 10^lnrow,
    n_features = 10^lncol
  )


scaling_preds <-
  models %>%
  select(method_id, predict_time, predict_mem) %>%
  rowwise() %>%
  do({
    df <- .
    exp <- scaling_exp
    exp$method_id <- df$method_id
    exp$time <- df$predict_time(exp$n_cells, exp$n_features)
    exp$mem <- df$predict_mem(exp$n_cells, exp$n_features)
    exp$logtime <- log10(exp$time)
    exp$logmem <- log10(exp$mem)
    exp
  }) %>%
  ungroup() %>%
  mutate(
    scaletime = (logtime - log10(10)) / (log10(3600 * 24 * 7) - log10(10)),
    scalemem = (logmem - log10(1e9)) / (log10(64e9) - log10(1e9)),
    scoretime = 1 - ifelse(scaletime > 1, 1, ifelse(scaletime < 0, 0, scaletime)),
    scoremem = 1 - ifelse(scalemem > 1, 1, ifelse(scalemem < 0, 0, scalemem)),
    timestr = label_time(time),
    memstr = label_memory(mem)
  )

scaling_agg <-
  scaling_preds %>%
  group_by(method_id) %>%
  summarise_at(c("scoretime", "scoremem"), mean)

scaling_scores <-
  bind_rows(
    scaling_preds %>% transmute(method_id, metric = paste0(labnrow, "_", labncol), scoretime, scoremem),
    scaling_agg %>% transmute(method_id, metric = "overall", scoretime, scoremem)
  )

write_rds(lst(scaling_preds, scaling_agg, scaling_scores), result_file("scaling_scores.rds"), compress = "xz")

# compare predictions versus reals
realdata <-
  data %>%
  filter(error_status == "no_error") %>%
  group_by(method_id, lnrow, lncol) %>%
  summarise(realtime = mean(time_method)) %>%
  ungroup() %>%
  mutate(logrealtime = log10(realtime))

compare <-
  left_join(scaling_preds %>% rename(predtime = time, logpredtime = logtime), realdata, by = c("method_id", "lnrow", "lncol")) %>%
  na.omit

g <-
  ggplot(compare, aes(logrealtime, logpredtime)) +
  geom_point() +
  ggrepel::geom_text_repel(aes(label = paste0(method_id, "\nlnrow=", lnrow, ", lncol=", lncol, "\nR=", label_time(realtime), ", P=", label_time(predtime))), compare %>% filter(abs(logpredtime - logrealtime) > log10(2))) +
  geom_abline(intercept = 0, slope = 1) +
  geom_abline(intercept = log10(2), slope = 1, colour = "red") +
  geom_abline(intercept = -log10(2), slope = 1, colour = "blue") +
  theme_bw()
ggsave(result_file("compare_predtime_vs_realtime.pdf"), g, width = 20, height = 20)

