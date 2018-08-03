library(dynbenchmark)
library(tidyverse)

experiment("04-method_characterisation/method_testing")

# generate default design
method_ids <- dynmethods::methods$id

design <- benchmark_generate_design(
  datasets = list(
    dyntoy::generate_dataset(id = "example_linear", model = "linear", num_cells = 99, num_features = 101),
    dyntoy::generate_dataset(id = "example_bifurcating", model = "bifurcating", num_cells = 99, num_features = 101),
    "real/developing-dendritic-cells_schlitzer",
    "real/fibroblast-reprogramming_treutlein"
  ),
  methods = method_ids
)

# also include dynmethods container examples
examples <- bind_rows(pbapply::pblapply(
  method_ids,
  cl = 8,
  function(method_id) {
    file <- paste0("../dynmethods/containers/", method_id, "/example.R")
    source(file)
    tibble(
      method_id = method_id,
      dataset = list(data),
      dataset_id = data$id,
      params = list(params),
      param_id = "example"
    )
  }
))
design$parameters <- bind_rows(
  design$parameters,
  examples %>% select(id = param_id, method_id, params)
)
design$crossing <- bind_rows(
  design$crossing,
  examples %>% mutate(prior_id = "none", repeat_ix = 1) %>% select(dataset_id, method_id, prior_id, repeat_ix, param_id)
)
design$datasets <- bind_rows(
  design$datasets,
  dynbenchmark:::process_datasets_design(examples$dataset)
)

# save and submit benchmark design
write_rds(design, derived_file("design.rds"))

benchmark_submit(
  design = design,
  qsub_grouping = "{method_id}",
  qsub_params = lst(timeout = 600, memory = "10G"),
  metrics = c("correlation", "edge_flip", "rf_rsq", "featureimp_cor")
)

# fetch the results, once they're in
benchmark_fetch_results()
output <- benchmark_bind_results(load_models = TRUE)

design <- read_rds(derived_file("design.rds"))



extract_method_status <- function(error_status, correlation, ...) {
  case_when(
    error_status != "no_error" ~ error_status,
    correlation < 0.5 ~ "low_correlation",
    TRUE ~ "success"
  )
}
output$method_status <- pmap_chr(output, extract_method_status)
output$dataset_id <- ifelse(grepl("_example", output$dataset_id), "personalised_example", output$dataset_id)


method_status_colors <- c(
  method_error = "#FF4136",
  execution_error = "#85144b",
  memory_limit = "#FF851B",
  time_limit = "#FFDC00",
  low_correlation = "#01FF70",
  success = "#2ECC40"
)
output %>%
  ggplot(aes(fct_rev(method_id), correlation)) +
    geom_label(aes(label = method_status, fill = method_status)) +
    coord_flip() +
    scale_fill_manual(values = method_status_colors) +
    scale_y_continuous(expand = c(0.5, 0)) +
    facet_wrap(~dataset_id)


output %>% filter(method_id == "cellrouter") %>% pull(stdout) %>% first() %>% cat

output %>% filter(method_id == "fateid") %>% pull(error_message) %>% cat

output %>% filter(str_detect(error_message, "no item called .*")) %>% pull(method_id) %>% unique()

output %>% filter(method_id == "urd", method_status == "method_error") %>% select(method_id, dataset_id, stdout, stderr, error_message) %>% pull(stdout)


# make sure all output is present
testthat::expect_setequal(output$method_id, design$method_id)


write_rds(output, derived_file("output.rds", experiment_id = "04-method_characterisation/method_testing"))
