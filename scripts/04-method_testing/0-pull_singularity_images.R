#' Update singularity images on the cluster

library(dynmethods)
library(dynwrap)
library(dynbenchmark)
library(tidyverse)

experiment("singularity_images")

# If nothing is running on the cluster, run this on the login node to clear temporary folders:
# rm -rf /tmp/* /data/*; mkdir /data/tmp
# for i in $(seq 1 8); do ssh prismcls0$i 'rm -rf /tmp/* /data/*; mkdir /data/tmp'; done

meth <- get_ti_methods()
handle <- qsub::qsub_lapply(
  X = meth$fun,
  qsub_environment = c(),
  qsub_packages = c("babelwhale"),
  qsub_config = qsub::override_qsub_config(
    max_wall_time = "01:00:00",
    name = "singbuild",
    memory = "10G",
    num_cores = 1,
    wait = FALSE,
    stop_on_error = FALSE
  ),
  FUN = function(fun) {
    fun()
  }
)
