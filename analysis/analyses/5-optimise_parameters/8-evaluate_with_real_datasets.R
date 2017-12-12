library(dynalysis)
library(tidyverse)
library(dynplot)

experiment("5-optimise_parameters/8-evaluate_with_real_datasets")

dataset_names <- list_datasets()
tasks <- pbapply::pblapply(dataset_names, load_dataset)
tasks[[31]]$trajectory_type <- "linear"
tasks <- tasks %>% list_as_tibble()

best_parm <- read_rds(result_file("best_params.rds", "5-optimise_parameters/7-train_parameters_with_synthetic_datasets"))

methods <- get_descriptions(as_tibble = F)
metrics <- "auc_R_nx"
timeout <- 300

mn <- "SCORPIUS"

param_sel <- best_parm %>% filter(method_name == mn) %>% dynutils::extract_row_to_list(1)
method <- methods[[mn]]

parameters <- param_sel$params
parameters <- lapply(names(parameters), function(prnm) {
  parset_par <- method$par_set$pars[[prnm]]
  parv <- parameters[[prnm]]
  if (!is.null(parset_par$trafo)) {
    parv <- parset_par$trafo(parv)
  }
  parv
}) %>% setNames(names(parameters))


eval_out <- execute_evaluation(
  tasks = tasks %>% slice(1),
  method = method,
  parameters = parameters,
  metrics = metrics,
  timeout = timeout,
  output_model = T,
  error_score = 0
)
extras <- attr(eval_out, "extras")
extras$.summary$error[[1]]
