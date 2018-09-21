library(dynbenchmark)
library(tidyverse)
library(GEOquery)
library(Biobase)
options('download.file.method.GEOquery'='curl')

id <- "real/gold/mESC-differentiation_hayashi"
dataset_preprocessing(id)

counts_location <- download_dataset_source_file(
  "GSE98664_tpm_sailfish_mergedGTF_RamDA_mESC_differentiation_time_course.txt.gz",
  "ftp://ftp.ncbi.nlm.nih.gov/geo/series/GSE98nnn/GSE98664/suppl/GSE98664_tpm_sailfish_mergedGTF_RamDA_mESC_differentiation_time_course.txt.gz"
)

counts_all <- read_tsv(counts_location) %>% as.data.frame %>% column_to_rownames("transcript_id") %>% as.matrix() %>% t %>% round()

cell_info_all <- tibble(
  cell_id = rownames(counts_all),
  milestone_id = gsub("RamDA_mESC_(.*)_.*", "\\1", cell_id)
)

milestone_network <- tribble(
  ~from, ~to, ~length,
  "00h", "12h", 1,
  "12h", "24h", 1,
  "24h", "48h", 2,
  "48h", "72h", 2
) %>% mutate(directed = TRUE)


milestone_ids <- unique(c(milestone_network$from, milestone_network$to))

cell_info <- slice(cell_info_all, match(rownames(counts_all), cell_id))
cell_info <- cell_info %>% filter(milestone_id %in% milestone_ids)
counts <- counts_all[cell_info$cell_id, ]

grouping <- cell_info %>% select(cell_id, milestone_id) %>% deframe()

save_raw_dataset(lst(milestone_network, cell_info, grouping, counts))
