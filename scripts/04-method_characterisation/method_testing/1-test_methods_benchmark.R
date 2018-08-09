library(dynbenchmark)
library(tidyverse)

experiment("04-method_characterisation/method_testing")

###################################################
###                   DESIGN                    ###
###################################################
method_ids <- dynmethods::methods$id

design <- benchmark_generate_design(
  datasets = list(
    dyntoy::generate_dataset(id = "toy/linear", model = "linear", num_cells = 99, num_features = 101),
    dyntoy::generate_dataset(id = "toy/bifurcating", model = "bifurcating", num_cells = 99, num_features = 101),
    "real/developing-dendritic-cells_schlitzer",
    "real/fibroblast-reprogramming_treutlein"
  ),
  methods = method_ids,
  parameters = list(
    fateid = tibble(id = "default", params = list(list(force = TRUE))),
    stemnet = tibble(id = "default", params = list(list(force = TRUE)))
  )
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
testthat::expect_equal(examples$dataset_id %>% str_replace_all("^specific_example/", ""), examples$method_id)

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

write_rds(design, derived_file("design.rds"))

###################################################
###                    SUBMIT                   ###
###################################################
design <- read_rds(derived_file("design.rds"))

benchmark_submit(
  design = design,
  qsub_grouping = "{method_id}",
  qsub_params = function(method_id) lst(timeout = 1200, memory = ifelse(method_id %in% c("ouija", "ouijaflow", "paga", "scimitar"), "32G", "10G")),
  metrics = c("correlation", "edge_flip", "rf_rsq", "featureimp_cor")
)

###################################################
###                    FETCH                    ###
###################################################
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

method_status_colors <- c(
  method_error = "#FF4136",
  execution_error = "#85144b",
  memory_limit = "#FF851B",
  time_limit = "#FFDC00",
  low_correlation = "#01FF70",
  success = "#2ECC40"
)
g <- output %>%
  mutate(dataset_id = gsub("specific_example/.*", "specific_example", dataset_id)) %>%
  ggplot(aes(correlation, fct_rev(method_id))) +
  geom_label(aes(label = method_status, fill = method_status)) +
  scale_fill_manual(values = method_status_colors) +
  scale_x_continuous(expand = c(0.5, 0)) +
  facet_wrap(~dataset_id, nrow = 1) +
  theme_bw()

ggsave(derived_file("method_status.pdf"), g, width = 16, height = 16)

#' @examples
#' output %>% filter(method_id == "cellrouter") %>% pull(stderr) %>% first() %>% cat
#' output %>% filter(method_id == "stemnet") %>% pull(stdout) %>% first() %>% cat
#'
#' output %>% filter(method_id == "pcreode", grepl("fibro", dataset_id)) %>% pull(stdout) %>% first() %>% cat
#'
#' output %>% filter(method_id == "projected_dpt", grepl("schl", dataset_id)) %>% pull(stderr) %>% first() %>% cat
#'
#' output %>% filter(method_id == "merlot") %>% pull(stdout) %>% cat
#'
#' output %>% filter(str_detect(error_message, "no item called .*")) %>% pull(method_id) %>% unique()
#'
#' output %>% filter(method_id == "urd", method_status == "method_error") %>% select(method_id, dataset_id, stdout, stderr, error_message) %>% pull(stdout)


write_rds(output, derived_file("output.rds", experiment_id = "04-method_characterisation/method_testing"))

checks <- output %>% group_by(method_id) %>% summarise(ran = mean(!method_status %in% c("method_error", "execution_error")))
write_rds(checks, derived_file("checks.rds", experiment_id = "04-method_characterisation/method_testing"))
