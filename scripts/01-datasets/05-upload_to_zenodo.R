#' Upload the datasets to Zenodo ([10.5281/zenodo.1211532](https://doi.org/10.5281/zenodo.1211532))

library(tidyverse)
library(dynbenchmark)
library(httr)

# authentication, get a token from the zenodo website with write access, and save it in .Renviron
access_token <- Sys.getenv("ZENODO_TOKEN")
testthat::expect_true(nchar(access_token) > 0)
headers <- add_headers(Authorization = paste("Bearer", access_token, sep = " "))

# test the connection
r <-  GET("https://zenodo.org/api/deposit/depositions", headers)
rawToChar(r$content) %>% rjson::fromJSON() %>% list_as_tibble()

# deposit id (see url of the deposit on the website)
deposit_id <- 1443566

# get bucket link
# this uses the new file API, as of 9 november 2018 not yet documented, see https://github.com/zenodo/zenodo/issues/940
# we use this new API because it can handle larger files + you can choose the destination path name
deposit <- GET(glue::glue("https://zenodo.org/api/deposit/depositions/{deposit_id}"), headers)
bucket_url <- content(deposit)$links$bucket

datasets <- list_datasets()

# check which files are uploaded
files <- GET(glue::glue("https://zenodo.org/api/deposit/depositions/{deposit_id}/files"), headers) %>% httr::content() %>% list_as_tibble()

uploaded_dataset_ids <- files$filename %>% gsub("\\.rds", "", .)

setdiff(datasets$id, uploaded_dataset_ids)


# choose which datasets to upload
dataset_ids_oi <- datasets$id
dataset_ids_oi <- setdiff(datasets$id, uploaded_dataset_ids)


#' @examples
#' dataset_id <- datasets$id[[1]]

library(qsub)
qsub_lapply(
  dataset_ids_oi,
  function(dataset_id) {
    library(dynbenchmark)
    library(tidyverse)
    library(httr)

    temp_dataset_file <- tempfile()

    dataset <- load_dataset(dataset_id)
    dataset$expression <- dynwrap::get_expression(dataset, "expression")
    dataset$counts <- dynwrap::get_expression(dataset, "counts")

    write_rds(dataset, temp_dataset_file, compress = "xz")

    path <- paste0(dataset_id, ".rds")
    PUT(
      glue::glue("{bucket_url}/{path}"),
      body = upload_file(temp_dataset_file),
      headers
    )

    file.remove(temp_dataset_file)
  }
)
