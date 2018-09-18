#' Gathers some metadata about all the real datasets

library(dynbenchmark)
library(googlesheets)

experiment("01-datasets/01-real")

gs_auth()

# download from google sheets
datasets_real_metadata <- gs_key("1SALZ2jt7TZJQJMEvvOwSR2r5yIl50qcGAZ-K5AC4DJo") %>%
  gs_read("included", col_types = list(pmid = col_character())) %>%
  tidyr::separate_rows(id, sep = ",\n")

# link the dataset ids to the metadata
datasets_ids_real <- list_datasets() %>% filter(source == "real") %>% pull(id)

datasets_real_metadata <- datasets_real_metadata %>%
  mutate(id = map(paste0("real/", datasets_real_metadata$id), str_match, string = datasets_ids_real) %>% map(~.[!is.na(.)])) %>%
  unnest(id)

if (any(!datasets_ids_real %in% datasets_real_metadata$id)) {
  stop("Not matched: ", setdiff(datasets_ids_real, datasets_real_metadata$id) %>% glue::glue_collapse(", "))
}

write_rds(datasets_real_metadata, result_file("metadata.rds"))

