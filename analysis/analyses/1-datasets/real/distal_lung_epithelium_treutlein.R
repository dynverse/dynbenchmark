rm(list=ls())
library(tidyverse)

txt_web_location <- "http://www.nature.com/nature/journal/v509/n7500/extref/nature13173-s4.txt"
txt_location <- "analysis/data/datasets_preproc/nature13173-s4.txt"

if (!file.exists(txt_location)) {
  download.file(paste0(txt_web_location), txt_location, method="libcurl")
}

expr <- read_tsv(txt_location)
expression <- expr[, -c(1:4)] %>% as.matrix() %>% magrittr::set_rownames(expr$cell_name)
cell_info <- expr[, c(1:4)] %>% as.data.frame() %>% magrittr::set_rownames(expr$cell_name) %>%
  rename(
    cell_id = cell_name,
    milestone_id = putative_cell_type
  ) %>% filter(sample == "singleCell")
expression <- expression[cell_info$cell_id, ]

info <- list(id="distal_lung_epithelium_treutlein")

# milestone_network <- tibble(from=cell_info$milestone_id, to=cell_info$milestone_id)
milestone_network = tribble(
  ~from, ~to,
  "BP", "AT1",
  "BP", "AT2"
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
