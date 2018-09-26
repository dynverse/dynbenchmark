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

    # predict time
    dat_time <- dat %>%
      filter(error_status %in% c("no_error", "time_limit")) %>%
      mutate(
        ltime = log10(ifelse(error_status == "time_limit", 3600, time_method))
      )
    model_time <- VGAM::vglm(ltime ~ lnrow + lncol, VGAM:::tobit(Upper = log10(3600), Lower = -5), data = dat_time)

    # reducing object size
    environment(model_time@terms$terms) <- NULL
    environment(model_time@misc$formula) <- NULL

    coef_values_time <- set_names(
      model_time@coefficients[c("(Intercept):1", "lnrow", "lncol")],
      c("time_intercept", "time_lnrow", "time_lncol")
    )

    # predict memory
    dat_mem <- dat %>%
      filter(error_status %in% c("no_error", "memory_limit")) %>%
      mutate(
        lmem = log10(ifelse(error_status == "memory_limit", 10 * 1e9, max_mem))
      )
    model_mem <- VGAM::vglm(lmem ~ lnrow + lncol, VGAM:::tobit(Upper = log10(10 * 1e9), Lower = 8), data = dat_mem)

    # reducing object size
    environment(model_mem@terms$terms) <- NULL
    environment(model_mem@misc$formula) <- NULL

    coef_values_mem <- set_names(
      model_mem@coefficients[c("(Intercept):1", "lnrow", "lncol")],
      c("mem_intercept", "mem_lnrow", "mem_lncol")
    )

    # calculate preds
    pred_ind <-
      datasets_info %>%
      select(dataset_id = id, lnrow, lncol) %>%
      mutate(
        method_id = dat$method_id[[1]],
        method_name = dat$method_name[[1]],
        time_lpred = predict(model_time, datasets_info)[,1],
        mem_lpred = predict(model_mem, datasets_info)[,1]
      )

    # format output
    dat %>%
      group_by(method_id, method_name) %>%
      summarise(pct_errored = mean(error_status != "no_error")) %>%
      ungroup() %>%
      mutate(
        model_time = list(model_time),
        model_mem = list(model_mem),
        pred_ind = list(pred_ind)
      ) %>%
      bind_cols(as.data.frame(t(c(coef_values_time, coef_values_mem)))) %>%
      mutate(
        time_lpred = mean(pred_ind[[1]]$time_lpred),
        mem_lpred = mean(pred_ind[[1]]$mem_lpred)
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
)

scaling_preds <-
  models %>%
  select(method_id, model_time) %>%
  rowwise() %>%
  do({
    df <- .
    exp <- scaling_exp
    exp$method_id <- df$method_id
    exp$logtime <- predict(df$model_time, exp)[,1]
    exp
  }) %>%
  ungroup() %>%
  mutate(
    scaletime = (logtime - log10(10)) / (log10(3600 * 24 * 7) - log10(10)),
    score = 1 - ifelse(scaletime > 1, 1, ifelse(scaletime < 0, 0, scaletime)),
    time = 10^logtime,
    timestr = case_when(
      time < 1 ~ "<1s",
      time < 60 ~ paste0(floor(time), "s"),
      time < 3600 ~ paste0(floor(time / 60), "m"),
      time < 3600 * 24 ~ paste0(floor(time / 3600), "h"),
      time < 3600 * 24 * 7 ~ paste0(floor(time / 3600 / 24), "d"),
      TRUE ~ ">7d"
    )
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
