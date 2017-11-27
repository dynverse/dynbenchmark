rm(list=ls())
library(tidyverse)
library(dynalysis)
options('download.file.method.GEOquery'='curl')

dataset_preprocessing("real", "aging_hsc_kowalczyk")

# download
file1 <- download_dataset_file("https://www.ncbi.nlm.nih.gov/geo/download/?acc=GSE59114&format=file&file=GSE59114%5FC57BL6%5FGEO%5Fall%2Exlsx", "GSE59114_C57BL6_GEO_all.xlsx")
# file2 <- download_dataset_file("https://www.ncbi.nlm.nih.gov/geo/download/?acc=GSE59114&format=file&file=GSE59114%5FDBA%5FGEO%5Fall%2Exlsx", "GSE59114_DBA_GEO_all.xlsx")


# read counts
tab1 <- readxl::read_xlsx(file1, skip = 1)
# tab2 <- readxl::read_xlsx(file2, skip = 1)

allexpression <- tab1 %>%
  select(-`UCSC transcripts`) %>%
  rename(gene = `Gene Symbol`) %>%
  gather(sample, count, -gene) %>%
  mutate(
    sample = gsub("^'", "", gsub("'$", "", sample)),
    gene = gsub("^'", "", gsub("'$", "", gene))
  ) %>%
  group_by(sample, gene) %>%
  summarise(count = sum(count)) %>%
  ungroup() %>%
  spread(gene, count) %>%
  as.data.frame()

allexpression <- allexpression %>% select(-sample) %>% magrittr::set_rownames(allexpression$sample) %>% as.matrix

# create cell info
allcell_info <- data_frame(
  cell_id = rownames(allexpression),
  age = ifelse(grepl("[yY]oung", cell_id), "young", ifelse(grepl("[oO]ld", cell_id), "old", "other")),
  milestone_id = ifelse(grepl("LT[-_]HSC", cell_id), "LT-HSC", ifelse(grepl("MPP", cell_id), "MPP", ifelse(grepl("ST[-_]HSC", cell_id), "ST-HSC", "other"))),
  repl = ifelse(grepl("[Rr]eplicate", cell_id), "rep1", "rep2"),
  bulk = !grepl("_[0-9]+$", cell_id)
) %>% filter(!bulk)


settings <- lapply(c("old", "young"), function(age) {
  list(
    id = pritt("aging_hsc_{age}_kowalczyk"),
    milestone_network = tribble(
      ~from, ~to,
      "LT-HSC", "ST-HSC",
      "ST-HSC", "MPP"
    ) %>% mutate(length = 1, directed = TRUE),
    ti_type = "linear",
    age = age
  )
})

for (setting in settings) {
  dataset_preprocessing("real", setting$id)

  milestone_network <- setting$milestone_network
  milestone_ids <- unique(c(milestone_network$from, milestone_network$to))

  cell_info <- allcell_info %>% filter(milestone_id %in% milestone_ids, age == setting$age)

  expression <- allexpression[cell_info$cell_id, ]
  cell_ids <- cell_info$cell_id

  cell_grouping <- cell_info %>% select(cell_id, group_id = milestone_id)
  milestone_percentages <- cell_info %>% select(cell_id, milestone_id) %>% mutate(percentage = 1)

  feature_info <- tibble(feature_id = colnames(expression))

  datasetpreproc_normalise_filter_wrap_and_save(
    dataset_prefix = datasetpreproc_getprefix(),
    dataset_id = datasetpreproc_getid(),
    ti_type = setting$ti_type,
    counts = 2^expression - 1, # todo: fix this
    cell_ids = cell_ids,
    milestone_ids = milestone_ids,
    milestone_network = milestone_network,
    milestone_percentages = milestone_percentages,
    cell_grouping = cell_grouping,
    cell_info = cell_info,
    feature_info = feature_info
  )
}
