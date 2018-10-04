#' Upload the datasets to Zenodo

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
# this uses the new file API, not yet documented, see https://github.com/zenodo/zenodo/issues/940
# we use this new API because it can handle larger files + you can choose the destination path name
deposit <- GET(glue::glue("https://zenodo.org/api/deposit/depositions/{deposit_id}"), headers)
bucket_url <- content(deposit)$links$bucket

path <- "ozewieze/wozewieze/wowo/kristalla.rds"
PUT(
  glue::glue("{bucket_url}/{path}"),
  body = upload_file("derived/01-datasets/real/gold/aging-hsc-old_kowalczyk/dataset.rds"),
  headers
)
