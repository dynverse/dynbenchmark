library(dynmethods)
library(dynwrap)
library(dynbenchmark)
library(tidyverse)

experiment("03-methods/singularity_images")


# RUN THIS ON THE LOGIN NODE:
#
# dir.create /data/tmp /data/singularity_cache /data/singularity_localcache /data/singularity_tmp
#
# Rscript -e 'purrr::map(dynmethods::repo_digests, dynwrap:::.container_pull_image, config = dynwrap::container_singularity(prebuild = FALSE))'
#
# rm /data/tmp/* /data/singularity_localcache/* /data/singularity_tmp/*
# for i in $(seq 1 8); do ssh prismcls0$i 'rm -rf /data/*'; done
# for i in $(seq 1 8); do rsync -av /data/singularity_cache prismcls0$i:/data; done
# for i in $(seq 1 8); do ssh prismcls0$i 'dir.create /data/tmp /data/singularity_cache /data/singularity_localcache /data/singularity_tmp'; done
# for i in $(seq 1 8); do ssh prismcls0$i 'rm -rf /tmp/.singula* /data/tmp/* /data/singularity_tmp/*'; done

# test with
# Rscript -e 'dynbenchmark::setup_singularity_methods(); dynwrap::infer_trajectory(dyntoy::generate_dataset(), dynmethods::ti_angle())'


handle <- qsub::qsub_lapply(
  X = dynmethods::repo_digests,
  qsub_environment = c(),
  qsub_packages = c("tidyverse", "dynmethods", "dynbenchmark"),
  qsub_config = qsub::override_qsub_config(
    max_wall_time = "01:00:00",
    name = "singbuild",
    memory = "10G",
    num_cores = 2,
    wait = FALSE,
    stop_on_error = FALSE
  ),
  FUN = function(dig) {
    config <- dynwrap::container_singularity(
      prebuild = TRUE,
      images_folder = derived_file("singularity_images/", experiment_id = "03-methods")
    )
    meth <- dynwrap:::.container_pull_image(dig, config = config)
  }
)
