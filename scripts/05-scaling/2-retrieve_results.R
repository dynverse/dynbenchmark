#' Retrieve the results and generate the scalability models

library(dynbenchmark)
library(tidyverse)
library(dynutils)
library(survival)

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

# If you want to download the output from prism
# qsub::rsync_remote(
#   remote_src = TRUE,
#   path_src = derived_file(remote = TRUE, experiment = "05-scaling"),
#   remote_dest = FALSE,
#   path_dest = derived_file(remote = FALSE, experiment = "05-scaling"),
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
#' data %>%
#'   group_by(method_id, error_status) %>%
#'   summarise(n = n()) %>%
#'   mutate(n = n / sum(n)) %>%
#'   ungroup() %>%
#'   reshape2::acast(method_id ~ error_status, value.var = "n", fill = 0) %>%
#'   pheatmap::pheatmap(cluster_cols = FALSE, cluster_rows = FALSE)

models <-
  data %>%
  as_tibble() %>%
  group_by(method_id) %>%
  filter(sum(error_status == "no_error") > 10) %>% # need at least a few data points
  do({
    dat <- .
    dat <- dat %>%
      filter(error_status %in% c("no_error")) %>%
      mutate(
        time = time_method,
        ltime = log10(time_method),
        mem = max_mem
      )

    # predict time
    model_time <- mgcv::gam(ltime ~ s(lnrow, lncol), data = dat)
    predict_time <- function(n_cells, n_features) {
      data <- data.frame(lnrow = log10(n_cells), lncol = log10(n_features))
      10^predict(model_time, data)
    }
    environment(predict_time) <- list2env(lst(model_time))

    # predict memory
    model_mem <- mgcv::gam(mem ~ s(lnrow, lncol), data = dat)
    predict_mem <- function(n_cells, n_features) {
      data <- data.frame(lnrow = log10(n_cells), lncol = log10(n_features))
      predict(model_mem, data)
    }
    environment(predict_mem) <- list2env(lst(model_mem))

    # calculate preds
    pred_ind <-
      datasets_info %>%
      select(dataset_id = id, nrow, ncol, lnrow, lncol) %>%
      mutate(
        method_id = dat$method_id[[1]],
        method_name = dat$method_name[[1]],
        time_pred = predict_time(nrow, ncol),
        mem_pred = predict_mem(nrow, ncol),
        time_lpred = log10(time_pred),
        mem_lpred = log10(mem_pred)
      )

    # format output
    dat %>%
      group_by(method_id, method_name) %>%
      summarise(pct_errored = mean(error_status != "no_error")) %>%
      ungroup() %>%
      mutate(
        predict_time = list(predict_time),
        predict_mem = list(predict_mem),
        pred_ind = list(pred_ind)
      ) %>%
      mutate(
        time_lpred = mean(log10(pred_ind[[1]]$time_pred)),
        mem_lpred = mean(log10(pred_ind[[1]]$mem_pred))
      )
  }) %>%
  ungroup()

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

scaling_exp <- tribble(
  ~ labnrow, ~ labncol, ~ lnrow, ~ lncol,
  "cells1k", "features10k", 3, 4,
  "cells10k", "features1k", 4, 3,
  "cells10k", "features10k", 4, 4,
  "cells10k", "features100k", 4, 5,
  "cells100k", "features10k", 5, 4
) %>% mutate(n_cells = 10^lnrow, n_features = 10^lncol)

scaling_preds <-
  models %>%
  select(method_id, predict_time) %>%
  rowwise() %>%
  do({
    df <- .
    exp <- scaling_exp
    exp$method_id <- df$method_id
    exp$time <- df$predict_time(exp$n_cells, exp$n_features)
    exp$logtime <- log10(exp$time)
    exp
  }) %>%
  ungroup() %>%
  mutate(
    scaletime = (logtime - log10(10)) / (log10(3600 * 24 * 7) - log10(10)),
    score = 1 - ifelse(scaletime > 1, 1, ifelse(scaletime < 0, 0, scaletime)),
    time = 10^logtime,
    timestr = label_time(time)
  )

scaling_agg <-
  scaling_preds %>%
  group_by(method_id) %>%
  summarise_at("score", mean)

scaling_scores <-
  bind_rows(
    scaling_preds %>% transmute(method_id, metric = paste0(labnrow, "_", labncol), score),
    scaling_agg %>% transmute(method_id, metric = "overall", score)
  )

write_rds(lst(scaling_preds, scaling_agg, scaling_scores), result_file("scaling_scores.rds"), compress = "xz")
