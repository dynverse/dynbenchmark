#' Download singularity images if TI methods on the cluster

library(dynmethods)
library(dynwrap)
library(dynbenchmark)
library(tidyverse)

experiment("singularity_images")


# If nothing is running on the cluster, run this on the login node to clear temporary folders:
# rm -rf /tmp/* /data/*
# for i in $(seq 1 8); do ssh prismcls0$i 'rm -rf /tmp/* /data/*'; done
# for i in $(seq 1 8); do ssh prismcls0$i 'mkdir /data/tmp'; done

methods <- dynmethods::methods

handle <- qsub::qsub_lapply(
  X = seq_len(nrow(methods)),
  qsub_environment = c("methods"),
  qsub_packages = c("babelwhale"),
  qsub_config = qsub::override_qsub_config(
    max_wall_time = "01:00:00",
    name = "singbuild",
    memory = "10G",
    num_cores = 1,
    wait = FALSE,
    stop_on_error = FALSE
  ),
  FUN = function(i) {
    meth <- babelwhale::pull_container(methods$docker_repository[[i]])
  }
)
