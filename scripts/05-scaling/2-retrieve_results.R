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
execution_output <- benchmark_bind_results(load_models = TRUE)
design <- read_rds(derived_file("design.rds"))
methods_info <- design$methods
datasets_info <- design$datasets

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
#' dat <- data %>% filter(method_id == "scorpius", error_status %in% c("no_error", "time_limit")) %>% filter(n() > 10)
#' data %>%
#'   group_by(method_id, error_status) %>%
#'   summarise(n = n()) %>%
#'   mutate(n = n / sum(n)) %>%
#'   ungroup() %>%
#'   reshape2::acast(method_id ~ error_status, value.var = "n", fill = 0) %>%
#'   pheatmap::pheatmap(cluster_cols = FALSE, cluster_rows = FALSE)

execution_time_models <-
  data %>%
  filter(error_status %in% c("no_error", "time_limit")) %>%
  as_tibble() %>%
  group_by(method_id) %>%
  filter(n() > 10) %>% # need at least a few data points
  do({
    dat <- .
    dat2 <- dat %>% mutate(
      ltime = log10(ifelse(error_status == "time_limit", 3600, time_method))
    )

    #model <- VGAM::vglm(ltime ~ lnrow + lncol + orig_dataset_id, VGAM:::tobit(Upper = log10(3600), Lower = -5), data = dat2)
    model <- VGAM::vglm(ltime ~ lnrow + lncol, VGAM:::tobit(Upper = log10(3600), Lower = -5), data = dat2)

    coef_values <- set_names(
      model@coefficients[c("(Intercept):1", "lnrow", "lncol")],
      c("intercept", "lnrow", "lncol")
    )

    lpredtimes <- datasets_info %>%
      select(dataset_id = id, lnrow, lncol) %>%
      mutate(
        method_id = dat$method_id[[1]],
        method_name = dat$method_name[[1]],
        lpredtime = predict(model, datasets_info)[,1]
      )

    dat %>%
      select(method_id, method_name) %>%
      unique() %>%
      mutate(
        model = list(model),
        predtimes = list(lpredtimes)) %>%
      bind_cols(as.data.frame(t(coef_values))) %>%
      mutate(lpredtime = mean(lpredtimes$lpredtime))
  }) %>%
  ungroup() %>%
  left_join(data %>% group_by(method_id) %>% summarise(pct_errored = mean(error_status != "no_error"), pct_timelimit = mean(error_status == "time_limit")), by = "method_id") %>%
  arrange(lpredtime) %>%
  mutate(
    method_id_f = factor(method_id, levels = method_id),
    method_name_f = factor(method_name, levels = method_name),
    y = -as.integer(method_id_f)
  )

data_pred <- bind_rows(execution_time_models$predtimes)
execution_time_models <- execution_time_models %>% select(-predtimes)

##########################################################
###############         SAVE DATA          ###############
##########################################################
write_rds(lst(data, data_pred, execution_time_models), result_file("scaling.rds"), compress = "xz")

