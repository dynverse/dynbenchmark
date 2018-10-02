library(dynbenchmark)
library(tidyverse)

experiment("06-benchmark/scaling_fit_cv")

# ###################################################
# ###                 FORMAT DATA                 ###
# ###################################################
#
# scaling <- read_rds(result_file("scaling.rds", "05-scaling"))
# benchmark_results_unnormalised <- read_rds(result_file("benchmark_results_unnormalised.rds", experiment_id = "06-benchmark"))
# benchmark_design <- read_rds(derived_file("design.rds", experiment_id = "06-benchmark"))
#
# max_memory <- 16 * 1e9
# max_time <- 60 * 60
#
# rep_levels <- c(paste0("training_", seq_len(5)), "predicted")
#
# training <-
#   scaling$data %>%
#   filter(error_status %in% c("no_error", "time_limit", "memory_limit")) %>%
#   transmute(
#     method_id,
#     replicate = factor(paste0("training_", match(orig_dataset_id, unique(orig_dataset_id))), levels = rep_levels),
#     lnrow,
#     lncol,
#     error_status,
#     ltime = case_when(error_status == "no_error" ~ log10(time_method), error_status == "time_limit" ~ log10(max_time), error_status == "memory_limit" ~ NA_real_),
#     lmemory = case_when(error_status == "no_error" ~ log10(max_mem), error_status == "time_limit" ~ NA_real_, error_status == "memory_limit" ~ log10(max_memory))
#   )
#
# validation <-
#   benchmark_results_unnormalised$raw_data %>%
#   left_join(benchmark_design$datasets %>% select(lnrow, lncol, dataset_id = id), by = "dataset_id") %>%
#   filter(error_status %in% c("no_error", "time_limit", "memory_limit")) %>%
#   transmute(
#     method_id,
#     replicate = factor("predicted", levels = rep_levels),
#     lnrow,
#     lncol,
#     error_status,
#     ltime = case_when(error_status == "no_error" ~ log10(time_method), error_status == "time_limit" ~ log10(max_time), error_status == "memory_limit" ~ NA_real_),
#     lmemory = case_when(error_status == "no_error" ~ log10(max_mem), error_status == "time_limit" ~ NA_real_, error_status == "memory_limit" ~ log10(max_memory))
#   )
#
# write_rds(training, derived_file("training.rds"))
# write_rds(validation, derived_file("validation.rds"))


###################################################
###               TRY A FEW MODELS              ###
###################################################
library(tidyverse)

experiment("06-benchmark/scaling_fit_cv")

training <- read_rds(derived_file("training.rds")) %>% filter(!method_id %in% c("error", "identity", "random", "shuffle"))
validation <- read_rds(derived_file("validation.rds")) %>% filter(!method_id %in% c("error", "identity", "random", "shuffle"))

max_lmemory <- log10(16 * 1e9)
max_ltime <- log10(60 * 60)


library(survival)
models <- list(
  "lm, lnrow + lncol" = function(train) lm(ltime ~ lnrow + lncol, train),
  "lm, lnrow + lncol + lnrow * lncol" = function(train) lm(ltime ~ lnrow + lncol + lnrow * lncol, train),
  "survreg left right, lnrow + lncol" = function(train) fit <- survival::survreg(survival::Surv(ltime, status) ~ lnrow + lncol, dist = "gaussian", train %>% mutate(status = case_when(error_status == "time_limit" ~ 0, ltime < 1 ~ 2, TRUE ~ 1))),
  "survreg right, lnrow + lncol" = function(train) fit <- survival::survreg(survival::Surv(ltime, status) ~ lnrow + lncol, dist = "gaussian", train %>% mutate(status = case_when(error_status == "time_limit" ~ 0, TRUE ~ 1))),
  "gam select, s(lnrow) + s(lncol)" = function(train) mgcv::gam(ltime ~ s(lnrow) + s(lncol), data = train, select = TRUE),
  "gam select, s(lnrow, lncol)" = function(train) mgcv::gam(ltime ~ s(lnrow, lncol), data = train, select = TRUE),
  "gam select, te(lnrow, lncol)" = function(train) mgcv::gam(ltime ~ te(lnrow, lncol), data = train, select = TRUE),
  "gam select, ti(lnrow) + ti(lncol) + ti(lnrow, lncol)" = function(train) mgcv::gam(ltime ~ ti(lnrow) + ti(lncol) + ti(lnrow, lncol), data = train, select = TRUE),
  "gam, s(lnrow) + s(lncol)" = function(train) mgcv::gam(ltime ~ s(lnrow) + s(lncol), data = train, select = FALSE),
  "gam, s(lnrow, lncol)" = function(train) mgcv::gam(ltime ~ s(lnrow, lncol), data = train, select = FALSE),
  "gam, te(lnrow, lncol)" = function(train) mgcv::gam(ltime ~ te(lnrow, lncol), data = train, select = FALSE),
  "gam, ti(lnrow) + ti(lncol) + ti(lnrow, lncol)" = function(train) mgcv::gam(ltime ~ ti(lnrow) + ti(lncol) + ti(lnrow, lncol), data = train, select = FALSE),
  "gam bscc, s(lnrow) + s(lncol)" = function(train) mgcv::gam(ltime ~ s(lnrow,bs="cc") + s(lncol, bs="cc"), data = train, select = TRUE)
)

###################################################
###             RUN CV ON TRAIN DATA            ###
###################################################
cvgrid <- crossing(
  method = unique(training$method_id %>% unique),
  replicate = paste0("training_", 1:5),
  model = names(models)
)

cvdf <- bind_rows(pbapply::pblapply(seq_len(nrow(cvgrid)), cl = 8, function(i) {
  mid <- cvgrid$method[[i]]
  repl <- cvgrid$replicate[[i]]
  mod <- cvgrid$model[[i]]

  train <- training %>% filter(method_id == mid, replicate != repl, lnrow + lncol <= 5)
  test <- training %>% filter(method_id == mid, replicate == repl)

  out <-
    tryCatch({
      model <- models[[mod]](train)
      predltime <- predict(model, test)

      lst(model, predltime)
    }, error = function(e) {
      lst(model = NULL, predltime = rep(NA, nrow(test)))
    })

  test$predltime <- out$predltime
  mse_test <- mean((test$ltime - test$predltime)^2, na.rm = TRUE)

  tibble(method_id = mid, replicate = repl, model = mod, mse_test, model_fit = list(out$model), test_data = list(test))
}))

###################################################
###           RUN MODELS ON VALID DATA          ###
###################################################
valgrid <- crossing(
  method = methods$id,
  model = names(models)
)
valdf <- bind_rows(pbapply::pblapply(seq_len(nrow(valgrid)), cl = 8, function(i) {
  mid <- valgrid$method[[i]]
  mod <- valgrid$model[[i]]

  train <- training %>% filter(method_id == mid)
  valid <- validation %>% filter(method_id == mid)

  out <-
    tryCatch({
      model <- models[[mod]](train)
      predltime <- predict(model, valid)

      lst(model, predltime)
    }, error = function(e) {
      lst(model = NULL, predltime = rep(0, nrow(valid)))
    })

  valid$predltime <- out$predltime
  mse_valid <- mean((valid$ltime - valid$predltime)^2, na.rm = TRUE)

  tibble(method_id = mid, model = mod, mse_valid, model_fit = list(out$model), valid_data = list(valid))
}))

###################################################
###                 COMBINE DATA                ###
###################################################
join <-
  left_join(
    cvdf,
    valdf,
    by = c("method_id", "model")
  )

###################################################
###                 COMBINE DATA                ###
###################################################
summary <-
  join %>%
  group_by(model) %>%
  summarise(mse_test = mean(mse_test, na.rm = TRUE), mse_valid = mean(mse_valid)) %>%
  arrange(mse_test)

write_rds(summary, result_file("summary.rds"), compress = "xz")
join2 <- join %>% select(-model_fit.x, -model_fit.y, -test_data, -valid_data) %>% as.data.frame()
write_rds(join2, result_file("join.rds"), compress = "xz")

