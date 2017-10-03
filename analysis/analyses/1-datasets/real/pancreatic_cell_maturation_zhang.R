rm(list=ls())
library(tidyverse)

txt_web_location <- "https://www.ncbi.nlm.nih.gov/geo/download/?acc=GSE87375&format=file&file=GSE87375%5FSingle%5FCell%5FRNA%2Dseq%5FGene%5FRead%5FCount%2Etxt%2Egz"
txt_location <- "analysis/data/datasets_preproc/GSE87375_Single_Cell_RNA-seq_Gene_Read_Count.txt.gz"

if (!file.exists(txt_location)) {
  download.file(paste0(txt_web_location), txt_location, method="libcurl") # libcurl muuuuuuuuuch faster, usualy
}

allcounts <- read_tsv(txt_location) %>% select(-Symbol) %>% column_to_rownames("ID") %>% as.matrix() %>% t

geo <- GEOquery::getGEO(GEO="GSE87375", destdir="analysis/data/datasets_preproc/")
allcell_info <- geo[[2]] %>% Biobase::phenoData() %>% as("data.frame") %>% rename(cell_id=title)
allcell_info <- allcell_info %>%
  mutate(milestone_id=gsub("batch: (.*)-\\d", "\\1", characteristics_ch1.5)) %>%
  select(cell_id, milestone_id) %>%
  mutate_all(funs(as.character))

settings <- list(
  list(
    info = list(id="pancreatic_alpha_cell_maturation_zhang"),
    milestone_network = tibble(
      from=c("α-cell E17.5", "α-cell P0", "α-cell P9", "α-cell P15", "α-cell P18"),
      to=c("α-cell P0", "α-cell P9", "α-cell P15", "α-cell P18", "α-cell P60"),
      length=(c(2.5, 9, 6, 3, 42)))
  ),
  list(
    info = list(id="pancreatic_beta_cell_maturation_zhang"),
    milestone_network = tibble(
      from=c("β-cell E17.5", "β-cell P0", "β-cell P3", "β-cell P9", "β-cell P15", "β-cell P18"),
      to=c("β-cell P0", "β-cell P3", "β-cell P9", "β-cell P15", "β-cell P18", "β-cell P60"),
      length=(c(2.5, 3, 6, 6, 3, 42)))
  )
)

for (i in 1:length(settings)) {
  list2env(settings[[i]], .GlobalEnv)

  milestone_ids <- unique(c(milestone_network$from, milestone_network$to))

  cell_info <- allcell_info %>% filter(milestone_id %in% milestone_ids)
  counts <- allcounts[cell_info$cell_id, ]
  cell_ids <- cell_info$cell_id

  cell_grouping <- cell_info %>% select(cell_id, milestone_id) %>% rename(group_id = milestone_id)
  milestone_percentages <- cell_grouping %>% rename(milestone_id=group_id) %>% mutate(percentage=1)

  gene_info <- tibble(id=colnames(counts))
  gene_ids <- gene_info$id

  expression <- log2(counts + 1)

  dataset <- lst(gene_info, cell_info, cell_grouping, cell_ids, gene_ids, counts, expression, milestone_network, milestone_ids, milestone_percentages, info)

  dynalysis:::save_dataset(dataset)
}
