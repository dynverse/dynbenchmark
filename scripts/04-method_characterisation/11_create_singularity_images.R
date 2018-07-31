library(dynmethods)
library(dynwrap)
library(dynbenchmark)
library(tidyverse)

# get methods
data("methods", package = "dynmethods")

setup_singularity_methods()

success <- map(
  methods$docker_repository,
  function(docker_repository) {
  tryCatch(
    {
      dynwrap::pull_singularity_ti_method(docker_repository)
      TRUE
    },
    error = function(e) e
  )
})
