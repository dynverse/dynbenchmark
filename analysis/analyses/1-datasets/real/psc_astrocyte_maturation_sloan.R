rm(list=ls())
library(tidyverse)

txt_web_location <- "https://www.ncbi.nlm.nih.gov/geo/download/?acc=GSE99951&format=file&file=GSE99951%5Fall%5Fdata%5Fhtseq%5Fout%2Ecsv%2Egz"
txt_location <- "analysis/data/datasets_preproc/GSE99951_all_data_htseq_out.csv"

if (!file.exists(txt_location)) {
  download.file(paste0(txt_web_location), txt_location, method="libcurl") # libcurl muuuuuuuuuch faster, usually
}

geo <- GEOquery::getGEO("GSE99951", destdir="analysis/data/datasets_preproc/")
cell_info_all <- geo[[1]] %>% Biobase::phenoData() %>% as("data.frame") %>% rename(cell_id=title, milestone_id=characteristics_ch1.3, group=characteristics_ch1.5) %>% mutate(cell_id=gsub("([^ ]*).*", "X\\1", cell_id)) %>% mutate_all(funs(as.character))

counts_all <- read.table(txt_location, TRUE, " ", stringsAsFactors = FALSE) %>% as.matrix() %>% t


settings <- list(
  list(
    id = "psc_astrocyte_maturation_neuron_sloan",
    group_id = "cell type: neuron"
  ),
  list(
    id = "psc_astrocyte_maturation_glia_sloan",
    group_id = "cell type: glia"
  )
)

for (setting in settings) {
  info <- list(technology = "smart_seq2", id=setting$id)

  milestone_network <- tribble(
    ~from, ~to,
    "age: Day 100", "age: Day 130",
    "age: Day 130", "age: Day 175",
    "age: Day 175", "age: Day 450"
  )

  milestone_ids <- unique(c(milestone_network$from, milestone_network$to))

  cell_info <- slice(cell_info_all, match(rownames(counts_all), cell_id))
  cell_info <- cell_info %>% filter(group == setting$group)
  cell_info <- cell_info %>% filter(milestone_id %in% milestone_ids)
  counts <- counts_all[cell_info$cell_id, ]
  cell_ids <- cell_info$cell_id

  cell_grouping <- cell_info %>% select(cell_id, milestone_id) %>% rename(group_id = milestone_id)
  milestone_percentages <- cell_grouping %>% rename(milestone_id=group_id) %>% mutate(percentage=1)

  gene_info <- tibble(id=colnames(counts))
  gene_ids <- gene_info$id

  expression <- log2(counts + 1)


  dataset <- lst(gene_info, cell_info, cell_grouping, cell_ids, gene_ids, counts, expression, milestone_network, milestone_ids, milestone_percentages, info)

  dynalysis:::save_dataset(dataset)
}

