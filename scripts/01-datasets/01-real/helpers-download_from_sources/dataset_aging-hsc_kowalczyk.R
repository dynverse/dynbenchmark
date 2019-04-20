library(tidyverse)
library(dynbenchmark)
options('download.file.method.GEOquery'='curl')

dataset_preprocessing("real/gold/aging-hsc_kowalczyk")

# download
file1 <- download_dataset_source_file("GSE59114_C57BL6_GEO_all.xlsx", "https://www.ncbi.nlm.nih.gov/geo/download/?acc=GSE59114&format=file&file=GSE59114%5FC57BL6%5FGEO%5Fall%2Exlsx")

# read counts
tab1 <- readxl::read_xlsx(file1, skip = 1)

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
allcounts <- round(2^allexpression - 1)

# create cell info
allcell_info <- data_frame(
  cell_id = rownames(allexpression),
  age = ifelse(grepl("[yY]oung", cell_id), "young", ifelse(grepl("[oO]ld", cell_id), "old", "other")),
  milestone_id = ifelse(grepl("LT[-_]HSC", cell_id), "LT-HSC", ifelse(grepl("MPP", cell_id), "MPP", ifelse(grepl("ST[-_]HSC", cell_id), "ST-HSC", "other"))),
  repl = ifelse(grepl("[Rr]eplicate", cell_id), "rep1", "rep2"),
  bulk = !grepl("_[0-9]+$", cell_id)
) %>% filter(!bulk)


settings <- map(c("old", "young"), function(age) {
  list(
    id = stringr::str_glue("real/gold/aging-hsc-{age}_kowalczyk"),
    milestone_network = tribble(
      ~from, ~to,
      "LT-HSC", "ST-HSC",
      "ST-HSC", "MPP"
    ) %>% mutate(length = 1, directed = TRUE),
    age = age
  )
})

for (setting in settings) {
  milestone_network <- setting$milestone_network

  cell_info <- allcell_info %>% filter(
    milestone_id %in% unique(c(milestone_network$from, milestone_network$to)),
    age == setting$age
  )

  grouping <- cell_info %>% select(cell_id, group_id = milestone_id) %>% deframe()

  counts <- allcounts[cell_info$cell_id, ]

  save_raw_dataset(
    lst(milestone_network, cell_info, grouping, counts),
    setting$id
  )
}
