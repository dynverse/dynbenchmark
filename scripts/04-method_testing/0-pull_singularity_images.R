library(dynmethods)
library(dynwrap)
library(dynbenchmark)
library(tidyverse)

experiment("singularity_images")


# If nothing is running on the cluster, run this on the login node to clear temporary folders:
# rm -rf /tmp/* /data/*
# for i in $(seq 1 8); do ssh prismcls0$i 'rm -rf /tmp/* /data/*'; done

method_ids <- dynmethods::methods$docker_repository

handle <- qsub::qsub_lapply(
  X = method_ids,
  qsub_environment = c(),
  qsub_packages = c("tidyverse", "dynmethods", "dynbenchmark"),
  qsub_config = qsub::override_qsub_config(
    max_wall_time = "01:00:00",
    name = "singbuild",
    memory = "10G",
    num_cores = 1,
    wait = FALSE,
    stop_on_error = FALSE
  ),
  FUN = function(method_id) {
    config <- dynwrap::container_singularity(
      prebuild = TRUE,
      images_folder = derived_file("", experiment_id = "singularity_images")
    )
    meth <- dynwrap:::.container_pull_image(method_id, config = config)
  }
)
