#' Test different models for scalability, using the running times from the benchmark as validation set

library(dynbenchmark)
library(tidyverse)

experiment("10-benchmark_interpretation/scaling_fit_cv")

###################################################
###                 FORMAT DATA                 ###
###################################################

max_memory <- 16 * 1e9
max_time <- 60 * 60
max_lmemory <- log10(max_memory)
max_ltime <- log10(max_time)

rep_levels <- c(paste0("training_", seq_len(5)), "predicted")

if (!file.exists(derived_file("training.rds"))) {
  scaling <- read_rds(result_file("scaling.rds", "05-scaling"))

  training <-
    scaling$data %>%
    filter(error_status %in% c("no_error", "time_limit", "memory_limit")) %>%
    transmute(
      method_id,
      replicate = factor(paste0("training_", match(orig_dataset_id, unique(orig_dataset_id))), levels = rep_levels),
      lnrow,
      lncol,
      error_status,
      ltime = case_when(error_status == "no_error" ~ log10(time_method), error_status == "time_limit" ~ log10(max_time), error_status == "memory_limit" ~ NA_real_),
      lmemory = case_when(error_status == "no_error" ~ log10(max_mem), error_status == "time_limit" ~ NA_real_, error_status == "memory_limit" ~ log10(max_memory))
    )

  write_rds(training, derived_file("training.rds"))
}

if (!file.exists(derived_file("validation.rds"))) {
  benchmark_results_unnormalised <- read_rds(result_file("benchmark_results_unnormalised.rds", experiment_id = "06-benchmark"))
  benchmark_design <- read_rds(derived_file("design.rds", experiment_id = "06-benchmark"))

  validation <-
    benchmark_results_unnormalised %>%
    left_join(benchmark_design$datasets %>% select(lnrow, lncol, dataset_id = id), by = "dataset_id") %>%
    filter(error_status %in% c("no_error", "time_limit", "memory_limit")) %>%
    transmute(
      method_id,
      replicate = factor("predicted", levels = rep_levels),
      lnrow,
      lncol,
      error_status,
      ltime = case_when(error_status == "no_error" ~ log10(time_method), error_status == "time_limit" ~ log10(max_time), error_status == "memory_limit" ~ NA_real_),
      lmemory = case_when(error_status == "no_error" ~ log10(max_mem), error_status == "time_limit" ~ NA_real_, error_status == "memory_limit" ~ log10(max_memory))
    )

  write_rds(validation, derived_file("validation.rds"))
}

###################################################
###               TRY A FEW MODELS              ###
###################################################
library(tidyverse)

experiment("10-benchmark_interpretation/scaling_fit_cv")

training <- read_rds(derived_file("training.rds")) %>% filter(!method_id %in% c("error", "identity", "random", "shuffle"))
validation <- read_rds(derived_file("validation.rds")) %>% filter(!method_id %in% c("error", "identity", "random", "shuffle"))






library(survival)
models <- list(
  "lm, lnrow + lncol" = function(train) lm(ltime ~ lnrow + lncol, train),
  "lm, lnrow + lncol + lnrow * lncol" = function(train) lm(ltime ~ lnrow + lncol + lnrow * lncol, train),
  "survreg right, lnrow + lncol" = function(train) fit <- survival::survreg(survival::Surv(ltime, status) ~ lnrow + lncol, dist = "gaussian", train %>% mutate(status = case_when(error_status == "time_limit" ~ 0, TRUE ~ 1))),
  "gam select, s(lnrow) + s(lncol)" = function(train) mgcv::gam(ltime ~ s(lnrow) + s(lncol), data = train, select = TRUE),
  "gam select, s(lnrow, lncol)" = function(train) mgcv::gam(ltime ~ s(lnrow, lncol), data = train, select = TRUE),
  "gam select, te(lnrow, lncol)" = function(train) mgcv::gam(ltime ~ te(lnrow, lncol), data = train, select = TRUE),
  "gam select, ti(lnrow) + ti(lncol) + ti(lnrow, lncol)" = function(train) mgcv::gam(ltime ~ ti(lnrow) + ti(lncol) + ti(lnrow, lncol), data = train, select = TRUE),
  "gam, s(lnrow) + s(lncol)" = function(train) mgcv::gam(ltime ~ s(lnrow) + s(lncol), data = train, select = FALSE),
  "gam, s(lnrow, lncol)" = function(train) mgcv::gam(ltime ~ s(lnrow, lncol), data = train, select = FALSE),
  "gam, te(lnrow, lncol)" = function(train) mgcv::gam(ltime ~ te(lnrow, lncol), data = train, select = FALSE),
  "gam, ti(lnrow) + ti(lncol) + ti(lnrow, lncol)" = function(train) mgcv::gam(ltime ~ ti(lnrow) + ti(lncol) + ti(lnrow, lncol), data = train, select = FALSE),
  "gam bscc, s(lnrow) + s(lncol)" = function(train) mgcv::gam(ltime ~ s(lnrow,bs="cc") + s(lncol, bs="cc"), data = train, select = TRUE),
  "scam, s(lnrow, lncol, bs = tedmi)" = function(train) scam::scam(ltime ~ s(lnrow, lncol, bs = "tedmi"), data = train),
  "glm" = function(train) {
    x.tr <- stats::model.matrix(~ lnrow + lncol + lnrow * lncol, data = train)[,-1]
    y.tr <- train$ltime
    fit <- glmnet::glmnet(x.tr, y.tr, alpha = 1)
  },
  "rf" = function(train) ranger::ranger(ltime ~ lnrow + lncol, data = train)
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

  train <- training %>% filter(method_id == mid, replicate != repl, lnrow + lncol <= 7, !is.na(ltime))
  test <- training %>% filter(method_id == mid, replicate == repl)

  out <-
    tryCatch({
      model <- models[[mod]](train)

      predltime <-
        if (mod == "rf") {
          predict(model, test)$predictions
        } else if (mod == "glm") {
          x.tr <- model.matrix(~ lnrow + lncol + lnrow * lncol, data = test)[,-1]
          predict(model, x.tr)[,1]
        } else {
          predict(model, test)
        }

      lst(model, predltime)
    }, error = function(e) {
      lst(model = NULL, predltime = rep(NA, nrow(test)))
    })

  test <- test %>%
    mutate(
      predltime = out$predltime,
      diff = ifelse(ltime >= max_ltime - .01 & predltime >= max_ltime - .01 , 0, ltime - predltime)
    )

  mse_test <- mean(test$diff^2, na.rm = TRUE)

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

  train <- training %>% filter(method_id == mid, !is.na(ltime))
  valid <- validation %>% filter(method_id == mid)

  out <-
    tryCatch({
      model <- models[[mod]](train)
      # predltime <- predict(model, valid)

      predltime <-
        if (mod == "rf") {
          predict(model, valid)$predictions
        } else if (mod == "glm") {
          x.tr <- model.matrix(~ lnrow + lncol + lnrow * lncol, data = valid)[,-1]
          predict(model, x.tr)[,1]
        } else {
          predict(model, valid)
        }


      lst(model, predltime)
    }, error = function(e) {
      lst(model = NULL, predltime = rep(0, nrow(valid)))
    })

  valid <- valid %>%
    mutate(
      predltime = out$predltime,
      diff = ifelse(ltime >= max_ltime - .01 & predltime >= max_ltime - .01 , 0, ltime - predltime)
    )

  mse_valid <- mean(valid$diff^2, na.rm = TRUE)

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
###               CALCULATE AUROC               ###
###################################################
valid_data <- map2_df(valdf$model, valdf$valid_data, function(model, data) {
  if (nrow(data) == 0) return(NULL)
  data$model <- model
  data
}) %>%
  filter(error_status %in% c("no_error", "time_limit"))

evals <- map(unique(valid_data$model), function(model) {
  dat <- valid_data %>% filter(model == !!model)
  eval <- GENIE3::evaluate_ranking_direct(
    values = dat$predltime,
    are_true = dat$error_status == "time_limit",
    num_positive_interactions = sum(dat$error_status == "time_limit"),
    num_possible_interactions = nrow(dat)
  )
  eval$metrics$name <- model
  eval$area_under$name <- model
  eval
})
eval <- list(
  metrics = map_df(evals, ~ .$metrics),
  area_under = map_df(evals, ~ .$area_under)
)

area_under <- eval$area_under %>%
  arrange(desc(F1)) %>%
  select(model = name, everything())
metrics <- eval$metrics %>% filter(name %in% area_under$model[1:12])

g1 <- ggplot(metrics, aes(1 - spec, tpr, colour = name)) + geom_step() + labs(x = "False positive rate", y = "Precision") + theme_bw() + theme(legend.position = "none") + scale_colour_brewer(palette = "Set3")
g2 <- ggplot(metrics, aes(tpr, prec, colour = name)) + geom_path() + labs(x = "Precision", y = "Recall") + theme_bw() + scale_colour_brewer(palette = "Set3")
g <- patchwork::wrap_plots(g1, g2, nrow = 1, widths = c(1, 1.15))
g
ggsave(result_file("auroc_aupr.pdf"), g, width = 12, height = 4)


###################################################
###                 COMBINE DATA                ###
###################################################
summary <-
  join %>%
  group_by(model) %>%
  summarise(mse_test = mean(mse_test, na.rm = TRUE), mse_valid = mean(mse_valid)) %>%
  arrange(mse_valid) %>%
  left_join(area_under, by = "model")

write_rds(summary, result_file("summary.rds"), compress = "xz")
join2 <- join %>% select(-model_fit.x, -model_fit.y, -test_data, -valid_data) %>% as.data.frame()
write_rds(join2, result_file("join.rds"), compress = "xz")


g <- ggplot(summary, aes(1 / mse_valid, F1)) + geom_point() + ggrepel::geom_text_repel(aes(label = model), nudge_y = .003) + theme_bw()
ggsave(result_file("mse_f1.pdf"), g, width = 8, height = 8)



summary %>% arrange(mse_test)
summary %>% arrange(mse_valid)
summary %>% arrange(desc(F1))
summary %>% arrange(mse_test * mse_valid / F1)


# > summary %>% arrange(mse_valid * mse_test / F1)
# # A tibble: 15 x 6
#    model                                                mse_test mse_valid auroc  aupr    F1
#    <chr>                                                   <dbl>     <dbl> <dbl> <dbl> <dbl>
#  1 gam, s(lnrow, lncol)                                   0.0483    0.0874 0.985 0.859 0.917
#  2 gam select, s(lnrow, lncol)                            0.0485    0.0873 0.985 0.859 0.917
#  3 rf                                                     0.111     0.0906 0.987 0.921 0.953
#  4 gam select, ti(lnrow) + ti(lncol) + ti(lnrow, lncol)   0.122     0.0870 0.984 0.858 0.917
#  5 gam select, s(lnrow) + s(lncol)                        0.100     0.106  0.981 0.857 0.915
#  6 gam, s(lnrow) + s(lncol)                               0.102     0.106  0.981 0.856 0.915
#  7 lm, lnrow + lncol + lnrow * lncol                      0.170     0.135  0.984 0.905 0.943
#  8 lm, lnrow + lncol                                      0.189     0.159  0.979 0.869 0.921
#  9 survreg right, lnrow + lncol                           0.189     0.183  0.978 0.878 0.925
# 10 gam select, te(lnrow, lncol)                           0.508     0.0871 0.984 0.857 0.916
# 11 gam bscc, s(lnrow) + s(lncol)                          0.381     0.130  0.977 0.853 0.911
# 12 scam, s(lnrow, lncol, bs = tedmi)                      0.576     0.0980 0.984 0.864 0.920
# 13 gam, ti(lnrow) + ti(lncol) + ti(lnrow, lncol)          0.776     0.0870 0.984 0.852 0.913
# 14 gam, te(lnrow, lncol)                                  0.913     0.0871 0.984 0.852 0.913
# 15 glm                                                    1.24      0.593  0.860 0.538 0.662
