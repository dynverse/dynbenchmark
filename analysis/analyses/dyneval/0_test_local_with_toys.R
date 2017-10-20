library(dynalysis)
library(tidyverse)
library(cowplot)

derived_dir <- "analysis/data/derived_data/dyneval/0_test_local_with_toys/"

# remove previous output
# unlink(derived_dir, recursive=TRUE)

dir.create(derived_dir, recursive = T)

###################### TOYS ########################
# # easy test
# tasks_file <- paste0(derived_dir, "tasks.RData")
# if (file.exists(tasks_file)) {
#   load(tasks_file)
# } else {
#   tasks <- generate_toy_datasets(num_replicates = 1)
#   task_group <- rep("group", nrow(tasks))
#   task_fold <- gsub(".*_", "", tasks$id) %>% as.integer()
#   save(tasks, task_group, task_fold, file = tasks_file)
# }

###################### TASKS ###########################
# dyngen folders
dyngen_derived_dir <- "analysis/data/derived_data/dyngen/"
.datasets_location <- paste0(dyngen_derived_dir, "4/")
tasks <- readRDS(paste0(dyngen_derived_dir, "tasks_v4.rds")) %>%
  filter(
    platform_id == "fluidigm_c1",
    takesetting_type == "snapshot")
task_group <- rep("group", nrow(tasks))
task_fold <- tasks$model_replicate
#######################################################

methods <- get_descriptions(as_tibble = T) %>% filter(name == "shuffle")

# metrics <- c("mean_R_nx", "auc_R_nx", "Q_local", "Q_global", "correlation",
#              "isomorphic", "ged", "robbie_network_score", "mantel_pval")
#metrics <- c("mean_R_nx", "auc_R_nx", "Q_local", "Q_global", "correlation")
#metrics <- c("ged", "isomorphic")
metrics <- "auc_R_nx"

# try executing eval first
method = extract_row_to_list(methods, 1)
out <- execute_evaluation(tasks = tasks, method = method, parameters = list(), metrics = metrics, timeout = 600, output_model = F)
extras <- attr(out, "extras")
attr(out, "extras") <- NULL
out

# try optimising
timeout = 600
num_cores = 8
num_repeats = 1
save_r2g_to_outdir = FALSE
num_iterations <- 5
num_init_params <- 16

## MBO settings
control_train <- mlrMBO::makeMBOControl(
  n.objectives = length(metrics),
  propose.points = num_cores,
  impute.y.fun = impute_y_fun(length(metrics))) %>%
  mlrMBO::setMBOControlTermination(iters = num_iterations)

if (length(metrics) == 1) {
  control_train <- control_train %>% mlrMBO::setMBOControlInfill(mlrMBO::makeMBOInfillCritCB())
} else {
  control_train <- control_train %>% mlrMBO::setMBOControlInfill(mlrMBO::makeMBOInfillCritDIB())
}
control_test <- control_train
control_test$iters <- 1
control_test$propose.points <- 1

# configure learner for predicting the performance of
# new param sets
mlr::configureMlr(show.learner.output = FALSE)
learner <- mlr::makeLearner(
  "regr.randomForest",
  se.method = "jackknife",
  predict.type = "se",
  keep.inbag = TRUE
)

## Run MBO
methodi <- 1
method <- dynutils::extract_row_to_list(methods, methodi)

# create an objective function
obj_fun <- make_obj_fun(method = method, metrics = metrics, timeout = timeout)

# generate initial parameters
design <- bind_rows(
  ParamHelpers::generateDesignOfDefaults(method$par_set),
  ParamHelpers::generateDesign(n = num_init_params, par.set = method$par_set)
)

fold_i <- 1

## start parameter optimisation
parallelMap::parallelStartMulticore(cpus = num_cores, show.info = TRUE)
tune_train <- mlrMBO::mbo(
  obj_fun,
  learner = learner,
  design = design,
  control = control_train,
  show.info = TRUE,
  more.args = list(
    tasks = tasks %>% slice(1:2),#tasks[task_fold != fold_i,],
    output_model = FALSE #"models/"
  )
)
tune_test <- mlrMBO::mbo(
  obj_fun,
  learner = learner,
  design = tune_train$opt.path$env$path %>% select(-starts_with("y_"), -one_of("y")),
  control = control_test,
  show.info = TRUE,
  more.args = list(
    tasks = tasks[task_fold == fold_i,],
    output_model = FALSE #"models/"
  )
)
parallelMap::parallelStop()


