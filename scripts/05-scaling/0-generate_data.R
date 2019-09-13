#' Generate up- and downscaled datasets

library(dynbenchmark)
library(tidyverse)

experiment("05-scaling")

##########################################################
###############      DEFINE DATASETS       ###############
##########################################################

# generate datasets with this range of dimensionality
scalability_range <- seq(log10(10), log10(1000000), by = log10(10) / 5)
print(round(10 ^ scalability_range))

# use helper function to generate datasets
source(scripts_file("generate_dataset.R"))

set.seed(1)

dataset_ids <- select_platforms(n_platforms = 5) %>% map_chr(~ .$platform_id)

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
num_cores <- 1
handle <- qsub::qsub_lapply(
  X = rev(seq_len(nrow(datasets))), # submit in reverse order so the first tasks will take the longest
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

#' @examples
#' download datasets from prism
#' qsub::rsync_remote(
#'   remote_src = TRUE,
#'   path_src = derived_file(experiment_id = "05-scaling/dataset", remote = TRUE),
#'   remote_dest = FALSE,
#'   path_dest = derived_file(experiment_id = "05-scaling/dataset", remote = FALSE),
#'   verbose = TRUE,
#'   compress = FALSE
#' )


