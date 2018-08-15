library(dynbenchmark)
library(tidyverse)

experiment("05-scaling")

##########################################################
###############      DEFINE DATASETS       ###############
##########################################################

# generate datasets with this range of dimensionality
# scalability_range <- log10(c(
#   10, 20, 40, 60, 80, 100,
#   100, 200, 400, 600, 800,
#   1000, 2000, 4000, 6000, 8000,
#   10000, 20000, 40000, 60000, 100000,
#   200000, 400000, 600000, 800000, 1000000
# ))
scalability_range <- seq(log10(10), log10(1000000), by = log10(10) / 5)
print(round(10 ^ scalability_range))

# use helper function to generate datasets
source(scripts_file("generate_dataset.R"))

dataset_ids <- c(
  "real/embronic-mesenchyme-neuron-differentiation_mca",
  "real/mouse-cell-atlas-combination-4",
  "real/kidney-collecting-duct-subclusters_park"
)

# construct datasets tibble
datasets <-
  crossing(
    orig_dataset_id = dataset_ids,
    lnrow = scalability_range,
    lncol = scalability_range
  ) %>%
  as_tibble() %>%
  mutate(
    nrow = ceiling(10 ^ lnrow),
    ncol = ceiling(10 ^ lncol),
    lsum = lnrow + lncol,
    memory = case_when(
      lsum >= 6 ~ 32,
      TRUE ~ 10
    )
  ) %>%
  filter(lsum <= 8) %>%
  arrange(lsum, orig_dataset_id, lnrow) %>%
  mutate(
    id = sprintf(paste0("scaling_%0", ceiling(log10(n())), "d"), seq_len(n())),
    type = "function",
    fun = map(id, ~ function() readr::read_rds(dynbenchmark::derived_file(c(., ".rds"), experiment_id = "05-scaling/dataset")))
  ) %>%
  select(id, type, fun, everything())

# create datasets and save at the remote's derived file folder
num_cores <- 4
handle <- qsub::qsub_lapply(
  X = rev(seq_len(nrow(datasets))),
  qsub_packages = c("tidyverse", "dynbenchmark", "dynwrap"),
  qsub_environment = c("datasets", "num_cores", "generate_dataset"),
  qsub_config = qsub::override_qsub_config(name = "datascaling", memory = "10G", num_cores = num_cores, wait = FALSE, max_wall_time = "12:00:00"),
  FUN = function(i) {
    filename <- dynbenchmark::derived_file(c(datasets$id[[i]], ".rds"), experiment_id = "05-scaling/dataset")
    # check whether dataset already exists
    if (file.exists(filename)) {
      success <-
        tryCatch({
          cat("Reading previous data file\n", sep = "")
          dataset <- readr::read_rds(filename)
          TRUE
        }, error = function(e) {
          FALSE
        })
      if (success) {
        cat("File already generated!\n", sep = "")
        return(TRUE)
      } else {
        cat("Could not read previous data file; starting again\n", sep = "")
        file.remove(filename)
      }
    }
    params <- as.list(datasets[i, ])
    dataset <- generate_dataset(
      orig_dataset_id = params$orig_dataset_id,
      lnrow = params$lnrow,
      lncol = params$lncol,
      cores = num_cores
    )

    readr::write_rds(dataset, filename, compress = "xz")

    TRUE
  }
)

readr::write_rds(datasets, path = derived_file("datasets.rds"), compress = "xz")
