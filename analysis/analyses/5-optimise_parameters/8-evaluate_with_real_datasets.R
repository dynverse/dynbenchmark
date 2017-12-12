library(dynalysis)
library(tidyverse)
library(dynplot)

experiment("5-optimise_parameters/8-evaluate_with_real_datasets")

dataset_names <- list_datasets()
tasks <- pbapply::pblapply(dataset_names, load_dataset) %>% list_as_tibble() %>%
  mutate(nrow = map_int(expression, nrow), ncol = map_int(expression, ncol))

tasks <- tasks %>% filter(nrow < 2000)

best_parm <- read_rds(result_file("best_params.rds", "5-optimise_parameters/7-train_parameters_with_synthetic_datasets")) %>%
  mutate(train_score = pmax(0, train_score), test_score = pmax(0, train_score))
selected_parms <- best_parm  %>% group_by(method_name, fold_i) %>% mutate(norm_score = test_score / mean(test_score)) %>%
  ungroup() %>% group_by(method_name) %>% arrange(desc(norm_score)) %>% slice(1) %>% ungroup() %>% arrange(method_name)

methods <- get_descriptions(as_tibble = F)
metrics <- "auc_R_nx"
timeout <- 300

method_names <- unique(selected_parms$method_name)

for (mn in method_names) {
  output_file <- derived_file(paste0(mn, ".rds"))

  if (!file.exists(output_file)) {
    cat("Running ", mn, "\n", sep = "")
    method <- methods[[mn]]
    parameters <- selected_parms %>% filter(method_name == mn) %>% dynutils::extract_row_to_list(1) %>% .$params
    parameters <- lapply(names(parameters), function(prnm) {
      parset_par <- method$par_set$pars[[prnm]]
      parv <- parameters[[prnm]]
      if (!is.null(parset_par$trafo)) {
        parv <- parset_par$trafo(parv)
      }
      parv
    }) %>% setNames(names(parameters))

    eval_out <- execute_evaluation(
      tasks = tasks,
      method = method,
      parameters = parameters,
      metrics = metrics,
      timeout = timeout,
      output_model = T,
      error_score = 0
    )
    write_rds(lst(parameters, eval_out), output_file)
  }
}
