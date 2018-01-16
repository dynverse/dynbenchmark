library(dynalysis)
library(tidyverse)

# set a seed
set.seed(1)

# generate a dataset. see ?generate_toy_datasets for the different kinds of models available
all_tasks <- read_rds(derived_file("tasks.rds", experiment_id = "5-optimise_parameters/11-evaluate_with_real_datasets-CORRELATION"))

tasks <- all_tasks %>% slice(1)
dataset <- dynutils::extract_row_to_list(tasks, 1)

milenet_gold <- dataset$milestone_percentages %>% reshape2::acast(cell_id ~ milestone_id, value.var = "percentage", fill = 0)

# plot the dataset
dynplot::plot_default(dataset)

methods <- dynmethods::get_descriptions(as_tibble = F)[c("SCORPIUS", "waterfll", "slngsht", "embeddr", "TSCAN", "CTVEM", "SCUBA")]

outs <- pbapply::pblapply(
  cl = 8,
# outs <- qsub_lapply(
  X = methods,
  # qsub_packages = c("tidyverse"),
  # qsub_environment = c("tasks", "dataset", "milenet_gold", "methods"),
  FUN = function(method) {
  tryCatch({
    # take the default parameters
    default_params <- ParamHelpers::generateDesignOfDefaults(method$par_set, trafo = TRUE) %>% ParamHelpers::dfRowToList(method$par_set, 1)

    # execute the method and evaluate it using the correlation of the pairwise geodesic distances
    eval <- dyneval::execute_evaluation(
      tasks = tasks,
      method = method,
      parameters = default_params,
      metrics = "correlation",
      timeout = 600
    ) %>%
      attr("extras")
    summary <- eval$.summary
    prediction <- eval$.models[[1]]

    milenet_prediction <- prediction$milestone_percentages %>% reshape2::acast(cell_id ~ milestone_id, value.var = "percentage", fill = 0)

    rfs <- lapply(seq_len(ncol(milenet_gold)), function(i) {
      randomForest::randomForest(milenet_prediction, milenet_gold[,i])
    })

    mses <- map_dbl(rfs, ~ mean(.$mse)) %>% setNames(colnames(milenet_gold))
    summary$mmse <- mean(mses)

    rsqs <- map_dbl(rfs, ~ mean(.$rsq)) %>% setNames(colnames(milenet_gold))
    summary$mrsq <- mean(rsqs)

    lst(default_params, summary, prediction, rfs)
  }, error = function(e) {
    NA
  })
})

summary <- outs %>% map_df(~.$summary)

ggplot(summary) + geom_point(aes(correlation, mrsq))

# look at the summary
summary

# plot the prediction
dynplot::plot_default(prediction)

# these should contain similar elements
names(dataset)
names(prediction)
