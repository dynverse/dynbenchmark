library(dynmethods)
library(dynwrap)
library(dynbenchmark)
library(tidyverse)

experiment("04-method_characterisation/singularity_images")

# get methods
data("methods", package = "dynmethods")

# build singularity images on the cluster
# use a trick with symbolic links so caches don't conflict between different runs
handle <- qsub::qsub_lapply(
  X = seq_len(nrow(methods)),
  qsub_environment = "methods",
  qsub_packages = c("tidyverse", "dynmethods", "dynbenchmark"),
  qsub_config = qsub::override_qsub_config(
    name = "singbuild",
    memory = "10G",
    num_cores = 6,
    wait = FALSE,
    stop_on_error = FALSE
  ),
  FUN = function(i) {
    docker_repository <- methods$docker_repository[[i]]
    method_id <- methods$id[[i]]

    # determine path location of global cache and local cache
    global_cache <- "/scratch/irc/shared/dynverse/.singularity_cache"
    cachedir <- paste0(global_cache, "_", method_id)
    tempdir <- paste0("/tmp/singularity_", method_id)

    # remove local cache
    if (dir.exists(cachedir)) {
      unlink(cachedir, recursive = TRUE)
    }

    # create cache folders
    dir.create(file.path(cachedir, "/docker"), recursive = TRUE, showWarnings = FALSE)
    dir.create(file.path(global_cache, "/docker"), recursive = TRUE, showWarnings = FALSE)

    # create link for every file in the global cache to the local cache
    cached_files <- list.files(file.path(global_cache, "/docker"))
    walk(cached_files, function(file) {
      cat("Lnking from global to local cache: ", file, "\n", sep = "")
      cache_file <- file.path(cachedir, "docker", file)
      global_file <- file.path(global_cache, "docker", file)
      file.symlink(global_file, cache_file)
    })

    # setup dynbenchmark to place images at the right directory
    dynbenchmark::setup_singularity_methods()

    # configure singularity to use correct cache and tempdirs
    Sys.setenv(SINGULARITY_CACHEDIR = cachedir)
    Sys.setenv(SINGULARITY_LOCALCACHEDIR = tempdir)

    # remove tempdir and local cache after singularity build
    on.exit(unlink(cachedir, recursive = TRUE))
    on.exit(unlink(tempdir, recursive = TRUE))

    # build singularity image
    cat("Building singularity image\n")
    dynwrap::pull_singularity_ti_method(docker_repository)

    # copy newly created files from the local cache to the global cache
    new_files <- setdiff(list.files(file.path(cachedir, "/docker")), cached_files)
    walk(new_files, function(file) {
      cat("Copying from local to global cache: ", file, "\n", sep = "")
      cache_file <- file.path(cachedir, "docker", file)
      global_file <- file.path(global_cache, "docker", file)

      # just to make sure, check whether the new file is not a symbolic link
      # and whether it does not exist yet in the global cache before copying
      if (Sys.readlink(cache_file) == "" && !file.exists(global_file)) {
        file.copy(cache_file, global_file)
      }
    })
  }
)

# copy files from cluster to local
qsub::rsync_remote(
  remote_src = TRUE,
  path_src = derived_file(remote = TRUE),
  remote_dest = FALSE,
  path_dest = derived_file(remote = FALSE),
  verbose = TRUE
)

