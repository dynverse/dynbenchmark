library(dynbenchmark)
library(tidyverse)
library(dynutils)
library(survival)

experiment("05-scaling")

##########################################################
###############      RETRIEVE RESULTS      ###############
##########################################################

# fetch results from cluster
benchmark_fetch_results(TRUE)

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
headtail <- function(X, num) {
  pbapply::pbsapply(X, cl = 8, function(x) {
    xs <- strsplit(x, split = "\n")[[1]]
    if (length(xs) > 2 * num) {
      xs <- c(head(xs, num), paste0("... ", length(xs) - 2 * num, " lines omitted ..."), tail(xs, num))
    }
    paste0(c(xs, ""), collapse = "\n")
  })
}
data$stdout <- headtail(data$stdout, 50)

write_rds(lst(data, data_pred, models), result_file("scaling.rds"), compress = "xz")

