rm(list=ls())
library(tidyverse)
library(dynalysis)

dataset_preprocessing("real", "neonatal_inner_ear_burns")


txt_web_location <- "https://www.ncbi.nlm.nih.gov/geo/download/?acc=GSE71982&format=file&file=GSE71982%5FRSEM%5FCounts%5FMatrix%2Etxt%2Egz"
count_location <- dataset_preproc_file("GSE71982_RSEM_Counts_Matrix.txt.gz")

if (!file.exists(count_location)) {
  download.file(txt_web_location, count_location, method="libcurl") # libcurl muuuuuuuuuch faster, usually
}

txt_web_location <- "https://www.ncbi.nlm.nih.gov/geo/download/?acc=GSE71982&format=file&file=GSE71982%5FP1%5FUtricle%5FPhenoData%2Etxt%2Egz"
phenodata_location <- dataset_preproc_file("GSE71982_P1_Utricle_PhenoData.txt.gz")

if (!file.exists(phenodata_location)) {
  download.file(txt_web_location, phenodata_location, method="libcurl")
}

counts_all <- read_tsv(count_location)[-c(1, 2),] %>% as.data.frame %>%  column_to_rownames("X1") %>% as.matrix() %>% t
colnames(counts_all) <- colnames(counts_all) %>% gsub("[\\\\\"]*([^\\\\\"]*)[\\\\\"]*", "\\1",.)
cell_info_all <- read_tsv(phenodata_location) %>% rename(cell_id=Short_Name, milestone_id=GroupID)

settings <- list(
  list(
    milestone_network = tribble(
      ~from, ~to,
      "TEC", "SC (i)",
      "SC (i)", "SC (ii)",
      "SC (ii)", "HC (ii)",
      "HC (ii)", "HC (iii-iv)",
      "TEC", "HC (i)",
      "HC (i)", "HC (iii-iv)"
    ) %>% mutate(length = 1, directed = TRUE),
    id = "neonatal_inner_ear_all_burns"
  ),
  list(
    milestone_network = tribble(
      ~from, ~to,
      "SC (i)", "SC (ii)",
      "SC (ii)", "HC (ii)",
      "HC (ii)", "HC (iii-iv)"
    ) %>% mutate(length = 1, directed = TRUE),
    id = "neonatal_inner_ear_SC_HC_burns"
  ),
  list(
    milestone_network = tribble(
      ~from, ~to,
      "TEC", "SC (i)",
      "SC (i)", "SC (ii)"
    ) %>% mutate(length = 1, directed = TRUE),
    id = "neonatal_inner_ear_TEC_SC_burns"
  ),
  list(
    milestone_network = tribble(
      ~from, ~to,
      "TEC", "HC (i)",
      "HC (i)", "HC (iii-iv)"
    ) %>% mutate(length = 1, directed = TRUE),
    id = "neonatal_inner_ear_TEC_HSC_burns"
  )
)

for (setting in settings) {
  dataset_preprocessing("real", setting$id)

  milestone_network <- setting$milestone_network

  milestone_ids <- unique(c(milestone_network$from, milestone_network$to))

  cell_info <- slice(cell_info_all, match(rownames(counts_all), cell_id))
  cell_info <- cell_info %>% filter(milestone_id %in% milestone_ids)
  counts <- counts_all[cell_info$cell_id, ]
  cell_ids <- cell_info$cell_id

  cell_grouping <- cell_info %>% select(cell_id, milestone_id) %>% rename(group_id = milestone_id)
  milestone_percentages <- cell_grouping %>% rename(milestone_id=group_id) %>% mutate(percentage=1)

  feature_info <- tibble(feature_id = colnames(counts))

  # todo: use dynutils normalisation
  expression <- log2(counts + 1)

  dataset <- wrap_ti_task_data(
    ti_type = "real",
    id = datasetpreproc_getid(),
    counts = counts,
    expression = expression,
    cell_ids = cell_ids,
    milestone_ids = milestone_ids,
    milestone_network = milestone_network,
    milestone_percentages = milestone_percentages,
    cell_grouping = cell_grouping,
    cell_info = cell_info,
    feature_info = feature_info
  )

  save_dataset(dataset)

}
