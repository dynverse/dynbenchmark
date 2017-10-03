rm(list=ls())
library(tidyverse)

txt_web_location <- "https://www.ncbi.nlm.nih.gov/geo/download/?acc=GSE67310&format=file&file=GSE67310%5FiN%5Fdata%5Flog2FPKM%5Fannotated%2Etxt%2Egz"
txt_location <- "analysis/data/datasets_preproc/GSE67310_iN_data_log2FPKM_annotated.txt.gz"

if (!file.exists(txt_location)) {
  download.file(paste0(txt_web_location), txt_location, method="libcurl") # libcurl muuuuuuuuuch faster, usualy
}

expr <- read_tsv(txt_location)
expression <- expr[, -c(1:5)] %>% as.matrix() %>% magrittr::set_rownames(expr$cell_name)
cell_info <- expr[, c(1:5)] %>% as.data.frame() %>% magrittr::set_rownames(expr$cell_name) %>%
  rename(
    cell_id = cell_name,
    milestone_id = assignment
  )

info <- list(id="fibroblast_reprogramming_treutlein")

milestone_network = tribble(
  ~from, ~to,
  "MEF", "d2_intermediate",
  "d2_intermediate", "d2_induced",
  "d2_induced", "d5_intermediate",
  "d5_intermediate", "d5_earlyiN",
  "d5_earlyiN", "Neuron",
  "d5_earlyiN", "Myocyte"
)
milestone_ids <- unique(c(milestone_network$from, milestone_network$to))

cell_info <- cell_info %>% filter(milestone_id %in% milestone_ids)
expression <- expression[cell_info$cell_id, ]
cell_ids <- cell_info$cell_id

cell_grouping <- cell_info %>% select(cell_id, milestone_id) %>% rename(group_id = milestone_id)
milestone_percentages <- cell_grouping %>% rename(milestone_id=group_id) %>% mutate(percentage=1)

gene_info <- tibble(id=colnames(expression))
gene_ids <- gene_info$id

dataset <- lst(gene_info, cell_info, cell_grouping, cell_ids, gene_ids, expression, milestone_network, milestone_ids, milestone_percentages, info)

dynalysis:::save_dataset(dataset)
