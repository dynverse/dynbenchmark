library(dynalysis)
library(tidyverse)

experiment("5-optimise_parameters/3-evaluate_parameters")

# settings
methods <- get_descriptions() %>% filter(short_name != "manual")

# use previous output to determine method ordering based on its running time
# outs <- read_rds("analysis/data/derived_data/5-optimise_parameters-old/3-evaluate_parameters/outputs_postprocessed.rds")
# outs$outputs_summmethod %>%
#   group_by(method_short_name) %>%
#   summarise_if(is.numeric, sum) %>%
#   arrange(time_method_mean) %>%
#   .$method_short_name %>%
#   paste("\"", ., "\"", collapse = ", ", sep = "") %>%
#   cat
methods_order <- c(
  "identity", "shuffle", "random", "slngsht", "scorpius", "mpath", "sincell", "embeddr", "waterfll",
  "dpt", "tscan", "slicer", "slice", "mnclica", "wndrlst", "mnclddr", "wishbone", "ctvem", "scuba",
  "topslam", "ouijaflw", "phenopth", "ctmaptpx", "mfa", "scoup"
)
methods <- methods %>% slice(c(match(methods_order, methods$short_name), which(!methods$short_name %in% methods_order)))
methods$short_name

metrics <- c("correlation", "rf_mse", "edge_flip")
extra_metrics <- c()
eval_timeout <- 60 * 60
optim_timeout <- 7 * 24 * 60 * 60
num_repeats <- 4

# read tasks
tasks <- read_rds(derived_file("tasks.rds", "5-optimise_parameters/0-process_tasks")) %>%
  mutate(nrow = map_int(expression, nrow), ncol = map_int(expression, ncol)) %>%
  filter(nrow < 2000)

# extract the default parameters
designs <- lapply(methods$short_name, function(mn) {
  par_set <- methods %>% filter(short_name == mn) %>% .$par_set %>% .[[1]]
  defaults <- ParamHelpers::generateDesignOfDefaults(par_set)
  # best <- ...
  # bind_rows(defaults, best)
  defaults
}) %>% setNames(methods$short_name)

# designs$manual <-
#   tribble(
#     ~person_id, ~dimred_id, ~run_i,
#     "wouters", "pca", 1,
#     "robrechtc", "mds", 1
#   )

# save benchmark configuration and start it
write_rds(lst(methods, designs, metrics, extra_metrics, num_repeats, tasks), derived_file("config.rds"))
bs_submit(
  tasks = tasks,
  # task_group = rep("task_group", nrow(tasks)),
  task_group = tasks$id,
  task_fold = rep(1, nrow(tasks)),
  out_dir = derived_file("suite/"),
  remote_dir = paste0("/scratch/irc/shared/dynverse_derived/", getOption("dynalysis_experiment_id"), "/"),
  methods = methods,
  designs = designs,
  metrics = metrics,
  extra_metrics = extra_metrics,
  memory = "10G",
  # num_cores = 4,
  num_cores = 1,
  num_iterations = 1,
  num_repeats = num_repeats,
  num_init_params = num_init_params,
  execute_before = "source /scratch/irc/shared/dynverse/module_load_R.sh; export R_MAX_NUM_DLLS=500; export DYNALYSIS_PATH=/group/irc/shared/dynalysis/",
  r_module = NULL,
  output_model = TRUE
)
