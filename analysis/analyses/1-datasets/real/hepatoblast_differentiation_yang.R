rm(list=ls())
library(tidyverse)

txt_web_location <- "https://www.ncbi.nlm.nih.gov/geo/download/?acc=GSE90047&format=file&file=GSE90047%5FSingle%2Dcell%5FRNA%2Dseq%5FTPM%2Etxt%2Egz"
txt_location <- "analysis/data/datasets_preproc/GSE90047_Single-cell_RNA-seq_TPM.txt.gz"

if (!file.exists(txt_location)) {
  download.file(paste0(txt_web_location), txt_location, method="libcurl") # libcurl muuuuuuuuuch faster, usually
}

counts <- read_tsv(txt_location) %>% select(-Symbol) %>% column_to_rownames("ID") %>% as.matrix() %>% t

geo <- GEOquery::getGEO(GEO="GSE90047", destdir="analysis/data/datasets_preproc/")
cell_info <- geo[[2]] %>% Biobase::phenoData() %>% as("data.frame") %>% rename(cell_id=title)
cell_info <- cell_info %>%
  mutate(day=gsub("embryonic day: (.*)", "\\1", characteristics_ch1), putative_cell_type=gsub("putative cell type: (.*)", "\\1", characteristics_ch1.4)) %>%
  select(cell_id, day, putative_cell_type) %>%
  mutate_all(funs(as.character))

cell_info <- cell_info %>%
  mutate(milestone_id = ifelse(day %in% c("E10.5", "E11.5", "E12.5"), day, paste0(day, "#", putative_cell_type)))

info <- list(organism = "mouse", genenames = "ensemble_gene", technology = "?", id="hepatoblast_differentiation_yang", normalization = "counts")

milestone_network <- tribble(
  ~from, ~to, ~length,
  "E10.5", "E11.5",1,
  "E11.5", "E12.5",1,
  "E12.5", "E13.5#hepatoblast/hepatocyte",1,
  "E12.5", "E13.5#cholangiocyte",1,
  "E13.5#cholangiocyte","E14.5#cholangiocyte",1,
  "E14.5#cholangiocyte","E15.5#cholangiocyte",1,
  "E15.5#cholangiocyte","E17.5#cholangiocyte",2,
  "E13.5#hepatoblast/hepatocyte","E14.5#hepatoblast/hepatocyte",1,
  "E14.5#hepatoblast/hepatocyte","E15.5#hepatoblast/hepatocyte",1,
  "E15.5#hepatoblast/hepatocyte","E17.5#hepatoblast/hepatocyte",2
)

milestone_ids <- unique(c(milestone_network$from, milestone_network$to))

cell_info <- slice(cell_info, match(rownames(counts), cell_id))
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
