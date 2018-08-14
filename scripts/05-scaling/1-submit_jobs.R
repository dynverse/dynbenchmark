library(dynbenchmark)
library(tidyverse)

experiment("05-scaling")

##########################################################
###############      DEFINE DATASETS       ###############
##########################################################

# generate datasets with this range of dimensionality
scalability_range <- log10(c(
  10, 20, 40, 60, 80, 100,
  100, 200, 400, 600, 800,
  1000, 2000, 4000, 6000, 8000,
  10000, 20000, 40000, 60000, 100000,
  200000, 400000, 600000, 800000, 1000000
))
# scalability_range <- seq(log10(10), log10(1000000), by = log10(10) / 5)
print(round(10 ^ scalability_range))

# use helper function to generate datasets
source(scripts_file("generate_dataset.R"))

# datasets <- load_datasets() %>%
#   mutate(
#     ncells = map_dbl(cell_ids, length),
#     nfeatures = map_dbl(feature_info, nrow)
#   )
# ggplot(datasets) + geom_point(aes(ncells, nfeatures, colour = source))
# datasets %>%
#   filter(source == "real", trajectory_type == "rooted_tree") %>%
#   select(id, ncells, nfeatures, trajectory_type) %>%
#   arrange(abs(ncells - 1000) * abs(nfeatures - 1000))

dataset_ids <- c(
  "real/embronic-mesenchyme-neuron-differentiation_mca",
  "real/mouse-cell-atlas-combination-4",
  "real/kidney-collecting-duct-subclusters_park"
)

make_dataset_function <- function(orig_dataset_id, lnrow, lncol) {
  f <- generate_dataset
  formals(f)$orig_dataset_id <- orig_dataset_id
  formals(f)$lnrow <- lnrow
  formals(f)$lncol <- lncol
  f
}

# construct datasets tibble
datasets <-
  crossing(
    orig_dataset_id = dataset_ids,
    lnrow = scalability_range,
    lncol = scalability_range
  ) %>%
  as_tibble() %>%
  mutate(
    id = sprintf(paste0("scaling_%0", ceiling(log10(n())), "d"), seq_len(n())),
    type = "function",
    fun = pmap(lst(orig_dataset_id, lnrow, lncol), make_dataset_function),
    nrow = ceiling(10 ^ lnrow),
    ncol = ceiling(10 ^ lncol),
    lsum = lnrow + lncol,
    memory = case_when(
      lsum >= 9 ~ 200,
      lsum >= 8 ~ 64,
      lsum >= 6 ~ 20,
      TRUE ~ 10
    )
  ) %>%
  select(id, type, fun, everything())

##########################################################
###############       DEFINE METHODS       ###############
##########################################################

# use method testing to define which methods will be run
checks <- read_rds(result_file("checks.rds", experiment_id = "04-method_testing"))
cat("NOT RUNNING: ", checks %>% filter(ran == 0) %>% pull(method_id) %>% paste(collapse = ", "), "\n", sep = "")

# use methods that were able to at least run on 1 dataset, and arrange them according to execution time
method_ids <- checks %>% filter(ran > 0) %>% arrange(time) %>% pull(method_id)
# method_ids <- "identity"

# construct methods tibble
methods <-
  dynwrap::get_ti_methods(method_ids, evaluate = FALSE) %>%
  mapdf(function(m) {
    l <- m$method_func()
    l$fun <- m$method_func
    l$type <- "function"
    l
  }) %>%
  list_as_tibble() %>%
  select(id, type, fun, everything())

##########################################################
###############       CREATE DESIGN        ###############
##########################################################
design <- benchmark_generate_design(
  datasets = datasets,
  methods = methods
)

design$crossing <- design$crossing %>%
  left_join(datasets %>% select(dataset_id = id, memory), by = "dataset_id") %>%
  mutate(method_order = match(method_id, method_ids)) %>%
  arrange(memory, method_order)

write_rds(design, derived_file("design.rds"), compress = "xz")


##########################################################
###############        SUBMIT JOB          ###############
##########################################################
design_filt <- read_rds(derived_file("design.rds"))

# only run the next stage when the first has finished
# design_filt$crossing <- design_filt$crossing %>% filter(memory < 20)
design_filt$crossing <- design_filt$crossing %>% filter(memory < 50)
# design_filt$crossing <- design_filt$crossing %>% filter(memory < 100)
# design_filt$crossing <- design_filt$crossing

benchmark_submit(
  design = design_filt,
  qsub_grouping = "{method_id}/{memory}",
  qsub_params = function(method_id, memory) list(timeout = 3600, memory = paste0(memory, "G")),
  metrics = list(dummy = function(dataset, model) 1),
  verbose = TRUE,
  output_models = FALSE
)

