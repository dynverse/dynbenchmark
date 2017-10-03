rm(list=ls())
library(tidyverse)

txt_web_location <- "https://www.ncbi.nlm.nih.gov/geo/download/?acc=GSE79363&format=file&file=GSE79363%5Ffirst%5Fdataset%5Fread%5Fcount%2Etxt%2Egz"
txt_location <- "analysis/data/datasets_preproc/GSE79363_first_dataset_read_count.txt.gz"

if (!file.exists(txt_location)) {
  download.file(paste0(txt_web_location), txt_location, method="libcurl") # libcurl muuuuuuuuuch faster, usualy
}

counts <- read_tsv(txt_location) %>% tibble::column_to_rownames("X1") %>% as.matrix() %>% t
cell_info <- tibble(cell_id=rownames(counts), milestone_id=rownames(counts) %>% gsub(".*_([A-Za-z]*)$", "\\1", .))

counts <- counts[cell_info$cell_id, ]

info <- list(id="macrophage_salmonella_saliba")

# milestone_network <- tibble(from=cell_info$milestone_id, to=cell_info$milestone_id)
milestone_network = tribble(
  ~from, ~to,
  "NNI", "MNGB",
  "NNI", "MGB",
  "NNI", "bystanders"
)
milestone_ids <- unique(c(milestone_network$from, milestone_network$to))

cell_info <- cell_info %>% filter(milestone_id %in% milestone_ids)
counts <- counts[cell_info$cell_id, ]
cell_ids <- cell_info$cell_id

cell_grouping <- cell_info %>% select(cell_id, milestone_id) %>% rename(group_id = milestone_id)
milestone_percentages <- cell_grouping %>% rename(milestone_id=group_id) %>% mutate(percentage=1)

gene_info <- tibble(id=colnames(counts))
gene_ids <- gene_info$id

expression <- log2(counts + 1)

dataset <- lst(gene_info, cell_info, cell_grouping, cell_ids, gene_ids, counts, expression, milestone_network, milestone_ids, milestone_percentages, info)

dynalysis:::save_dataset(dataset)
