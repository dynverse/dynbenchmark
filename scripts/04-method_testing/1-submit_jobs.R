#' Submit jobs for method testing

library(dynbenchmark)
library(tidyverse)

experiment("04-method_testing")

if (!file.exists(derived_file("design.rds"))) {
  ###################################################
  ###                   DESIGN                    ###
  ###################################################

  methods <- dynwrap::get_ti_methods(evaluate = TRUE) %>%
    mutate(method_type = type, type = "fun")

  design <- benchmark_generate_design(
    datasets = list(
      dyntoy::generate_dataset(id = "toy/linear", model = "linear", num_cells = 99, num_features = 101),
      dyntoy::generate_dataset(id = "toy/bifurcating", model = "bifurcating", num_cells = 99, num_features = 101),
      "real/gold/developing-dendritic-cells_schlitzer",
      "real/silver/fibroblast-reprogramming_treutlein"
    ),
    methods = methods %>% select(id, type, fun, version),
    parameters = list(
      fateid = tibble(id = "default", force = TRUE),
      stemnet = tibble(id = "default", force = TRUE),
      tscan = tibble(id = "default", modelNames = list(c("VVV", "EEE")))
    )
  )

  # also include dynmethods container examples
  examples <- bind_rows(pbapply::pblapply(
    seq_len(nrow(methods)),
    cl = 8,
    function(i) {
      method_id <- methods$id[[i]]
      image <- methods$docker_repository[[i]]

      ex <- dynwrap:::.container_get_example(image)
      tibble(
        method_id = method_id,
        dataset = list(ex$data),
        dataset_id = ex$data$id,
        params = list(ex$params),
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

  write_rds(design, derived_file("design.rds"), compress = "xz")
}

###################################################
###                    SUBMIT                   ###
###################################################
design <- read_rds(derived_file("design.rds"))

# # try first with one method:
# design$crossing <- design$crossing %>% filter(method_id == "angle")

benchmark_submit(
  design = design,
  qsub_grouping = "{method_id}",
  qsub_params = function(method_id) lst(timeout = 1200, memory = ifelse(method_id %in% c("ouija", "ouijaflow", "paga", "scimitar"), "32G", "16G")),
  metrics = c("correlation", "edge_flip", "featureimp_cor", "featureimp_wcor", "F1_branches", "him")
)

