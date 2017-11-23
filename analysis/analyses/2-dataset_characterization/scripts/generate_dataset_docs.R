library(tidyverse)
library(googlesheets)

dataset_infos <- gs_title("Real datasets") %>%
  gs_read(ws = "datasets", col_types = cols(date = col_date())) %>%
  separate_rows(id, sep=",")

all_files <- list.files("data", rec=F, full.names=T)
dataset_files <- all_files[!is.na(file.info(all_files)$isdir)]
dataset_ids <- dataset_files %>% gsub("data/(.*).rda", "\\1", .)

# look into dynalysis::load_dataset?
load_dataset <- function(dataset_id) {
  load(paste0("data/", dataset_id, ".rda"))
  get(dataset_id)
}

dataset_docs <- parallel::mclapply(dataset_ids, function(dataset_id) {
  print(paste0("-------------", dataset_id))
  print("loading")
  dataset <- load_dataset(dataset_id)

  dataset_info <- dataset_infos %>% filter(id == dataset_id)

  contains <- pritt("`{names(dataset)}`") %>% paste0(collapse=", ")

  list(
    pritt("#' {dataset$info$id}"),
    pritt("#' "),
    pritt("#' Expression dataset of {nrow(dataset$expression)} cells and {ncol(dataset$expression)} features."),
    pritt("#' "),
    pritt("#' @format list containing {contains}"),
    pritt("#' @source dataset_info$gse"),
    pritt("\"{dataset_info$id}\""),
    pritt("")
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
#   save(list=c(dataset$info$id), file=pritt("data/{dataset$info$id}.rda"))
# }, mc.cores=6)

