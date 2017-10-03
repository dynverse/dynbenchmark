library(tidyverse)
library(glue)
library(googlesheets)

dataset_infos <- gs_title("Real datasets") %>%
  gs_read(ws = "datasets", col_types = cols(date = col_date())) %>%
  separate_rows(id, sep=",")

all_files <- list.files("data", rec=F, full.names=T)
dataset_files <- all_files[!is.na(file.info(all_files)$isdir)]
dataset_ids <- dataset_files %>% gsub("data/(.*).rda", "\\1", .)

load_dataset <- function(dataset_id) {
  load(paste0("data/", dataset_id, ".rda"))
  get(dataset_id)
}

dataset_docs <- parallel::mclapply(dataset_ids, function(dataset_id) {
  print(paste0("-------------", dataset_id))
  print("loading")
  dataset <- load_dataset(dataset_id)

  dataset_info <- dataset_infos %>% filter(id == dataset_id)

  contains <- glue("`{names(dataset)}`") %>% paste0(collapse=", ")

  list(
    glue("#' {dataset$info$id}"),
    glue("#' "),
    glue("#' Expression dataset of {nrow(dataset$expression)} cells and {ncol(dataset$expression)} features."),
    glue("#' "),
    glue("#' @format list containing {contains}"),
    glue("#' @source dataset_info$gse"),
    glue("\"{dataset_info$id}\""),
    glue("")
  )
}, mc.cores=8)

dataset_docs %>% unlist() %>% paste0(collapse="\n") %>% write("R/data.R")

#
#
# dataset_docs <- parallel::mclapply(dataset_files, function(dataset_file) {
#   print(paste0("-------------", dataset_file))
#   print("loading")
#   load(dataset_file)
#   assign(dataset$info$id, dataset)
#
#   save(list=c(dataset$info$id), file=glue("data/{dataset$info$id}.rda"))
# }, mc.cores=6)

