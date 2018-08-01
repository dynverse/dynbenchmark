library(dynbenchmark)
library(tidyverse)

experiment("04-method_characterisation/method_testing")

design <- generate_benchmark_design(
  datasets = c(
    "toy/bifurcating_1",
    "toy/linear_1",
    "real/developing-dendritic-cells_schlitzer",
    "real/fibroblast-reprogramming_treutlein"
  ),
  methods = dynmethods::methods$id,
  give_priors = NULL
)
write_rds(design, derived_file("design.rds"))

design <- read_rds(derived_file("design.rds"))

local_output_folder <- derived_file("suite")
remote_output_folder <- derived_file("suite", remote = T)

benchmark_submit(design, local_output_folder = local_output_folder, remote_output_folder = remote_output_folder)
benchmark_fetch_results(local_output_folder)
output <- benchmark_bind_results(local_output_folder, load_models = TRUE)




extract_method_status <- function(error_status, correlation, ...) {
  case_when(
    error_status != "no_error" ~ error_status,
    correlation < 0.5 ~ "low_correlation",
    TRUE ~ "success"
  )
}
output$method_status <- pmap_chr(output, extract_method_status)


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



# make sure all output is present
testthat::expect_setequal(output$method_id, design$method_id)


write_rds(output, derived_file("output.rds", experiment_id = "04-method_characterisation/method_testing"))
