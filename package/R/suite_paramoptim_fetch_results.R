#' Fetch the results of the benchmark jobs from the cluster.
#'
#' @param remote The host at which to check for running jobs.
#'   If \code{NULL}, each qsub handle will be checked individually.
#'   If \code{TRUE}, the default qsub handle will be used.
#' @param local_output_folder A folder in which to output intermediate and final results.
#'
#'
#' @importFrom readr read_rds write_rds
#' @importFrom mlrMBO mboFinalize
#'
#' @export
paramoptim_fetch_results <- function(
  remote = NULL,
  local_output_folder = derived_file("suite")
) {
  requireNamespace("qsub")

  # find all 2nd level folders with individual tasks
  handles <- list.files(local_output_folder, pattern = "qsubhandle.rds", recursive = TRUE, full.names = TRUE)

  # check for running job ids
  running_job_ids <-
    if (!is.null(remote)) {
      qsub::qstat_remote(remote = remote) %>%
      gsub("^ *([0-9]*).*", "\\1", .) %>%
      unique() %>%
      keep(~ . != "")
    } else {
      c()
    }

  # process each method separately
  map(handles, function(handle) {
    name <- handle %>% gsub(paste0(local_output_folder, "/"), "", ., fixed = TRUE) %>% gsub("/qsubhandle.rds", "", ., fixed = TRUE)
    output_metrics_file <- gsub("qsubhandle.rds", "output_metrics.rds", handle, fixed = TRUE)

    if (!file.exists(handle)) {
      cat(name, ": No qsub file was found.\n", sep = "")
      return(FALSE)
    }

    if (file.exists(output_metrics_file)) {
      cat(name, ": Output already present.\n", sep = "")
      return(FALSE)
    }

    cat(name, ": Attempting to retrieve output from cluster. ", sep = "")
    metadata <- readr::read_rds(handle)
    subdesign <- metadata$subdesign
    qsub_handle <- metadata$qsub_handle
    num_datasets <- qsub_handle$num_datasets

    if (qsub_handle$job_id %in% running_job_ids) {
      cat("Job is still running.\n")
      return(FALSE)
    }

    # attempt to retrieve results; return NULL if job is still busy or has failed
    output <- qsub::qsub_retrieve(
      qsub_handle,
      wait = FALSE
    )

    ## Create summary from the last saved states
    summ_fun <- function() {
      file <- paste0(qsub_handle$src_dir, "/mlrmbo/mlr_progress.RData")

      if (file.exists(file)) {
        load(file)
        opt.state$opt.problem$control$save.file.path <- file
        save(opt.state, file = file)
        mlr_out <- mlrMBO::mboFinalize(file)

        dob <- mlr_out$opt.path$env$dob
        path <- mlr_out$opt.path$env$path %>% select(-one_of(metadata$metrics))
        extra <- mlr_out$opt.path$env$extra

        map_df(seq_along(dob), function(param_i) {
          extra[[param_i]]$.summary  %>%
            mutate(
              repeat_i = grid$repeat_i[[grid_i]],
              grid_i,
              iteration = dob[[param_i]],
              param_i,
              param_row = list(path[param_i,,drop = F]),
              error_message = sapply(error, function(err) ifelse(is.null(err), "", err$message))
            ) %>%
            select(-error)
        })
      } else {
        # todo: mimic normal output
        return(NULL)
      }
    }
    outputs <- summ_fun()

    # save output
    readr::write_rds(outputs, output_metrics_file)

    return(TRUE)
  })

  # return nothing
  invisible()
}


#' Gather and bind the results of the benchmark jobs
#'
#' @param filter_fun A function with which to filter the data as it is being read from files. Function must take a single data frame as input and return a filtered data frame as a result.
#' @param local_output_folder A folder in which to output intermediate and final results.
#'
#' @importFrom readr read_rds
#' @export
paramoptim_bind_results <- function(
  filter_fun = NULL,
  local_output_folder = derived_file("suite")

) {
  # find all 2nd level folders with individual tasks
  files <- list.files(local_output_folder, pattern = "output_metrics.rds", recursive = TRUE, full.names = TRUE)

  # process each method separately
  as_tibble(map_df(files, function(output_metrics_file) {
    name <- output_metrics_file %>%
      gsub(paste0(local_output_folder, "/"), "", ., fixed = TRUE) %>%
      gsub("/output_metrics.rds", "", ., fixed = TRUE)

    if (file.exists(output_metrics_file)) {
      cat(name, ": Reading previous output\n", sep = "")
      output <- readr::read_rds(output_metrics_file)

      if (!is.null(filter_fun)) {
        output$before_filter_index <- seq_len(nrow(output))
        output <- output %>% filter_fun()
      }

      output
    } else {
      cat("Output not found, skipping", name, "\n", sep = "")
      NULL
    }
  }))
}


