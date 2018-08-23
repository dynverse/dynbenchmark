library(dynmethods)
library(dynwrap)
library(dynbenchmark)
library(tidyverse)

experiment("03-method_characterisation/singularity_images")

# get methods
data("methods", package = "dynmethods")

# build singularity images on the cluster
# use a trick with symbolic links so caches don't conflict between different runs
handle <- qsub::qsub_lapply(
  X = seq_len(nrow(methods)),
  qsub_environment = "methods",
  qsub_packages = c("tidyverse", "dynmethods", "dynbenchmark"),
  qsub_config = qsub::override_qsub_config(
    max_wall_time = "01:00:00",
    name = "singbuild",
    memory = "10G",
    num_cores = 4,
    wait = FALSE,
    stop_on_error = FALSE
  ),
  FUN = function(i) {
    remote_digests <- methods$remote_digests[[i]]
    method_id <- methods$id[[i]]

    # determine path location of global cache and local cache
    global_cache <- "/scratch/irc/shared/dynverse/singularity_cache/global"
    cachedir <- paste0("/scratch/irc/shared/dynverse/singularity_cache/", method_id)
    tempdir <- paste0("/data/singularity_tmp/singularity/", method_id)

    # remove local cache
    if (dir.exists(cachedir)) {
      unlink(cachedir, recursive = TRUE)
    }
    if (dir.exists(cachedir)) {
      unlink(tempdir, recursive = TRUE)
    }

    # create cache folders
    dir.create(file.path(cachedir, "/docker"), recursive = TRUE, showWarnings = FALSE)
    dir.create(file.path(global_cache, "/docker"), recursive = TRUE, showWarnings = FALSE)
    dir.create(file.path(tempdir, "/docker"), recursive = TRUE, showWarnings = FALSE)

    # remove tempdir and local cache after singularity build
    on.exit(unlink(cachedir, recursive = TRUE))
    on.exit(unlink(tempdir, recursive = TRUE))

    # create link for every file in the global cache to the local cache
    cached_files <- list.files(file.path(global_cache, "/docker"))
    walk(cached_files, function(file) {
      cat("Linking from global to local cache: ", file, "\n", sep = "")
      cache_file <- file.path(cachedir, "docker", file)
      global_file <- file.path(global_cache, "docker", file)
      file.symlink(global_file, cache_file)
    })


    # configure singularity to use correct cache and tempdirs
    Sys.setenv(SINGULARITY_CACHEDIR = cachedir)
    Sys.setenv(SINGULARITY_LOCALCACHEDIR = tempdir)
    Sys.setenv(SINGULARITY_TMPDIR = tempdir)
    Sys.setenv(TEMPDIR = tempdir) # not taking any chances

    # build singularity image
    cat("Building singularity image\n")
    dynwrap::create_ti_method_with_container(
      image = remote_digests[[1]],
      config = container_singularity(derived_file("singularity_images/", experiment_id = "03-method_characterisation"))
    )

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

qsub::qsub_retrieve(handle)

#' @examples
#' # copy files from cluster to local
#' qsub::rsync_remote(
#'   remote_src = TRUE,
#'   path_src = derived_file(remote = TRUE),
#'   remote_dest = FALSE,
#'   path_dest = derived_file(remote = FALSE),
#'   verbose = TRUE
#' )
#'
#' qsub::run_remote("for i in $(seq 1 8); do ssh prismcls0$i 'rm -rf /data/singularity_*'; done")
