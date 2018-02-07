library(dynalysis)
library(tidyverse)
library(dynplot)

experiment("5-optimise_parameters/1-try_toys")

unlink(derived_file("", "5-optimise_parameters/1-try_toys"), recursive = TRUE, force = TRUE)

# collect 4 toy datasets
tasks <- read_rds(derived_file("tasks.rds", "5-optimise_parameters/0-process_tasks")) %>%
  filter(task_group == "toy") %>%
  sample_n(4)

# settings
methods <- get_descriptions()
metrics <- c("correlation", "rf_mse", "edge_flip")

# extract the default parameters
designs <- lapply(methods$par_set, function(par_set) {
  ParamHelpers::generateDesignOfDefaults(par_set)
}) %>% setNames(methods$short_name)

# save benchmark configuration and start it
benchmark_suite_submit(
  tasks = tasks,
  task_group = tasks$task_group,
  task_fold = rep(1, nrow(tasks)),
  out_dir = derived_file("suite/"),
  remote_dir = paste0("/scratch/irc/shared/dynverse_derived/", getOption("dynalysis_experiment_id"), "/"),
  methods = methods,
  designs = designs,
  metrics = metrics,
  memory = "10G",
  num_cores = 4,
  num_iterations = 1,
  num_repeats = 1,
  num_init_params = 1,
  execute_before = "source /scratch/irc/shared/dynverse/module_load_R.sh; export R_MAX_NUM_DLLS=500",
  r_module = NULL,
  output_model = TRUE
)

outputs <- benchmark_suite_retrieve(derived_file("suite/"))
#
#
# # process output
# outputs2 <- outputs %>%
#   rowwise() %>%
#   mutate(
#     memory = ifelse(!is.null(qacct), qacct$maxvmem, NA)
#   ) %>%
#   ungroup()
#
# tmp <- outputs2 %>%
#   rowwise() %>%
#   mutate(
#     qsub_error = ifelse(qsub_error != "all parameter settings errored", qsub_error, individual_scores$error[[1]]$message),
#     qsub_len = str_length(qsub_error),
#     qsub_error = str_sub(qsub_error, qsub_len-600, qsub_len)
#   ) %>%
#   ungroup() %>%
#   select(method_name, qsub_error, which_errored)
# for (i in seq_len(nrow(tmp))) {
#   cat("METHOD ", tmp$method_name[[i]], " ERRORED BECAUSE OF: \n", sep = "")
#   cat(tmp$qsub_error[[i]], "\n\n", sep = "")
# }
#
# eval_ind <-
#   bind_rows(succeeded$individual_scores) %>%
#   rowwise() %>%
#   mutate(error_message = ifelse(is.null(error), "", error$message)) %>%
#   ungroup()
#
# eval_ind$error_message

