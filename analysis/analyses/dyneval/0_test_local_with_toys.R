library(dynalysis)
library(tidyverse)
library(cowplot)

derived_dir <- "analysis/data/derived_data/dyneval/0_test_local_with_toys/"

# remove previous output
# unlink(derived_dir, recursive=TRUE)

dir.create(derived_dir, recursive = T)

# easy test
tasks_file <- paste0(derived_dir, "tasks.RData")
if (file.exists(tasks_file)) {
  load(tasks_file)
} else {
  tasks <- generate_toy_datasets(num_replicates = 1)
  task_group <- rep("group", nrow(tasks))
  task_fold <- gsub(".*_", "", tasks$id) %>% as.integer()
  save(tasks, task_group, task_fold, file = tasks_file)
}
methods <- get_descriptions(as_tibble = T)

# metrics <- c("mean_R_nx", "auc_R_nx", "Q_local", "Q_global", "correlation",
#              "isomorphic", "ged", "robbie_network_score", "mantel_pval")

#metrics <- c("mean_R_nx", "auc_R_nx", "Q_local", "Q_global", "correlation")
metrics <- c("ged", "isomorphic")

# try executing eval first
method = extract_row_to_list(methods, 1)
out <- execute_evaluation(tasks = tasks, method = method, parameters = list(), metrics = metrics, timeout = 600)
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
  mlrMBO::setMBOControlTermination(iters = num_iterations) %>%
  mlrMBO::setMBOControlInfill(mlrMBO::makeMBOInfillCritDIB())
control_test <- control_train
control_test$iters <- 1
control_test$propose.points <- 1
learner <- mlr::makeLearner(
  "regr.randomForest",
  se.method = "jackknife",
  predict.type = "se",
  keep.inbag = TRUE
)

## Grid settings
grid <- expand.grid(
  fold_i = sort(unique(task_fold)),
  group_sel = sort(unique(task_group)),
  repeat_i = seq_len(num_repeats),
  stringsAsFactors = FALSE
)

# create an objective function
obj_fun <- make_obj_fun(method = method, metrics = metrics, timeout = timeout)

# generate initial parameters
design <- bind_rows(
  ParamHelpers::generateDesignOfDefaults(method$par_set),
  ParamHelpers::generateDesign(n = num_init_params, par.set = method$par_set)
)

grid_i <- 1
fold_i <- grid[grid_i,]$fold_i
group_sel <- grid[grid_i,]$group_sel
repeat_i <- grid[grid_i,]$repeat_i

# try once more, before param optim
parameters <- extract_row_to_list(design, 1)
obj_out <- obj_fun(parameters, tasks)
extras <- attr(obj_out, "extras")
attr(obj_out, "extras") <- NULL
obj_out

## start parameter optimisation
# parallelMap::parallelStartMulticore(cpus = num_cores, show.info = TRUE)
tune_train <- mlrMBO::mbo(
  obj_fun,
  learner = learner,
  design = design,
  control = control_train,
  show.info = TRUE,
  more.args = list(tasks = tasks)
)
# parallelMap::parallelStop()

