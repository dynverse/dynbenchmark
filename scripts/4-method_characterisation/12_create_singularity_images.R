library(dynmethods)
library(dynwrap)
library(dynbenchmark)
library(tidyverse)

experiment("4-method_characterisation/singularity_images")

# get methods
data("methods", package = "dynmethods")
methods <- methods %>% filter(containerised)

options(dynwrap_singularity_images_folder = derived_file())

success <- map(
  methods$docker_container,
  function(docker_container) {
  tryCatch(
    {
      dynwrap::pull_singularity_ti_method(docker_container, singularity_images_folder = singularity_images_folder)
      TRUE
    },
    error = function(e) e
  )
})

# create images on cluster
handle <- qsub_lapply(
  methods$docker_container[1:2],
  function(docker_container) {
    tryCatch(
      {
        dynwrap::pull_singularity_ti_method(docker_container, singularity_images_folder = remote_singularity_images_folder)
        TRUE
      },
      error = function(e) e
    )
  },
  qsub_environment = "remote_singularity_images_folder",
  qsub_config = qsub_config
)

qsub_retrieve(handle)

qsub_lapply(1:2, function(x) tempdir())
