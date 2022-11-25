#' Run the methods on the cluster

library(dynbenchmark)
library(tidyverse)

experiment("05-scaling")

options(dynwrap_backend = "r_wrapper")

if (!file.exists(derived_file("design.rds"))) {
  ##########################################################
  ###############      DEFINE DATASETS       ###############
  ##########################################################
  datasets <-
    readr::read_rds(derived_file("datasets.rds")) %>%
    mutate(
      category = case_when(
        lsum <= 5 ~ "Cat1",
        lsum <= 6 ~ "Cat2",
        lsum <= 7 ~ "Cat3",
        TRUE ~ "Cat4"
      )
    )

  ##########################################################
  ###############       DEFINE METHODS       ###############
  ##########################################################

  # use method testing to define which methods will be run
  checks <- read_rds(result_file("checks.rds", experiment_id = "04-method_testing"))
  cat("NOT RUNNING: ", checks %>% filter(ran == 0) %>% pull(method_id) %>% paste(collapse = ", "), "\n", sep = "")

  # use methods that were able to at least run on 1 dataset, and arrange them according to execution time
  method_ids <- checks %>% filter(ran > 0) %>% arrange(time) %>% pull(method_id)

  # construct methods tibble
  methods <-
    dynwrap::get_ti_methods(method_ids, evaluate = FALSE) %>%
    mapdf(function(m) {
      l <- m$fun()
      k <- list()
      for (n in names(l)) {
        for (p in names(l[[n]])) {
          k[[paste0(n, "_", p)]] <- l[[n]][[p]]
        }
      }
      k$fun <- m$fun
      k$type <- "function"
      k
    }) %>%
    list_as_tibble() %>%
    select(id = method_id, type, fun, everything())

  ##########################################################
  ###############       CREATE DESIGN        ###############
  ##########################################################
  design <- benchmark_generate_design(
    datasets = datasets,
    methods = methods,
    parameters = list(
      fateid = tibble(id = "default", force = TRUE),
      stemnet = tibble(id = "default", force = TRUE),
      tscan = tibble(id = "default", modelNames = list(c("VVV", "EEE")))
    )
  )

  design$crossing <- design$crossing %>%
    left_join(datasets %>% select(dataset_id = id, category), by = "dataset_id") %>%
    mutate(method_order = match(method_id, method_ids)) %>%
    arrange(category, method_order)

  write_rds(design, derived_file("design.rds"), compress = "xz")
}

##########################################################
###############        SUBMIT JOB          ###############
##########################################################
design_filt <- read_rds(derived_file("design.rds"))

# only run the next stage when the first has finished
design_filt$crossing <- design_filt$crossing %>% filter(method_id == "scorpius")
# design_filt$crossing <- design_filt$crossing %>% filter(category == "Cat1", method_id %in% c("angle", "paga"))
# design_filt$crossing <- design_filt$crossing %>% filter(category == "Cat1")
# design_filt$crossing <- design_filt$crossing %>% filter(category %in% c("Cat1", "Cat2"))
# design_filt$crossing <- design_filt$crossing %>% filter(category %in% c("Cat1", "Cat2", "Cat3"))

benchmark_submit(
  design = design_filt,
  qsub_grouping = "{method_id}/{category}",
  qsub_params = list(timeout = 3600, memory = "16G"),
  metrics = list(dummy = function(dataset, model) 1),
  verbose = TRUE,
  output_models = FALSE
)

