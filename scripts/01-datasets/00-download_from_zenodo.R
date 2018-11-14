#' Downloading the processed datasets from Zenodo ([10.5281/zenodo.1443566](https://doi.org/10.5281/zenodo.1443566))

library(tidyverse)
library(dynbenchmark)
library(httr)

experiment("01-datasets")

# download the rds files from zenodo
deposit_id <- 1443566
files_list <- GET(glue::glue("https://zenodo.org/api/deposit/depositions/{deposit_id}/files"), headers) %>%
  httr::content()

files <- tibble(
  filename = map_chr(files_list, "filename")
  # url = map(files_list, "links") %>% map_chr("download") # this url does not work
) %>%
  mutate(
    url = glue::glue("https://zenodo.org/record/{deposit_id}/files/{filename}"),
    destfile = map_chr(filename, derived_file)
  )

# download one file
download.file(files$url[[1]], files$destfile[[1]])

# download all files
map2(
  files$url,
  files$destfile,
  download.file
)

# upload to remote, if relevant
# this assumes you have a gridengine cluster at hand
qsub::rsync_remote(
  remote_src = FALSE,
  path_src = derived_file(),
  remote_dest = TRUE,
  path_dest = derived_file(remote = TRUE)
)
