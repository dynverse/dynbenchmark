#' Downloading the processed datasets from Zenodo ([10.5281/zenodo.1443566](https://doi.org/10.5281/zenodo.1443566))

library(tidyverse)
library(dynbenchmark)
library(httr)
library(Matrix)

experiment("01-datasets")

# download the rds files from zenodo
deposit_id <- 1443566

files <-
  GET(glue::glue("https://zenodo.org/api/records/{deposit_id}")) %>%
  httr::content() %>%
  .$files %>%
  map(unlist) %>%
  list_as_tibble() %>%
  mutate(
    dataset_id = str_replace(filename, "\\.rds$", ""),
    dest_folder = map_chr(dataset_id, derived_file)
  )

pwalk(
  files,
  function(checksum, filename, filesize, links.download, dataset_id, dest_folder, ...) {
    download_info <- paste0(dest_folder, "/download_info.json")
    if (file.exists(download_info)) {
      didata <- jsonlite::read_json(download_info, simplifyVector = TRUE)

      if (didata$checksum == checksum) {
        return()
      }
    }

    tmp_download_file <- tempfile()
    on.exit(file.remove(tmp_download_file))
    download.file(links.download, tmp_download_file)

    if (tools::md5sum(tmp_download_file) != checksum) {
      stop("MD5 checksum after downloading file '", dataset_id, "' is wrong!\nAborting downloading procedure.")
    }

    dataset <- read_rds(tmp_download_file)
    dataset$expression <- as(dataset$expression, "dgCMatrix")
    dataset$counts <- as(dataset$counts, "dgCMatrix")

    save_dataset(dataset, id = dataset_id)
    jsonlite::write_json(
      lst(
        deposit_id,
        dataset_id,
        checksum,
        downloaded_on = as.character(Sys.time())
      ),
      download_info
    )
  }
)

# upload to remote, if relevant
# this assumes you have a gridengine cluster at hand
qsub::rsync_remote(
  remote_src = FALSE,
  path_src = derived_file(),
  remote_dest = TRUE,
  path_dest = derived_file(remote = TRUE)
)
