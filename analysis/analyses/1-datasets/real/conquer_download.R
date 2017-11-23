rm(list=ls())
library(tidyverse)

requireNamespace("MultiAssayExperiment")
source("data-raw/helpers/conquer_infos.R")

names(conquer_infos)

for (conquer_id in c("cell_cycle_buettner", "human_embryos_petropoulos", "NKT_differentiation_engel", "cell_cycle_leng", "mesoderm_development_loh")) {
  source_info <- conquer_infos[[conquer_id]]

  rds_location <- paste0("analysis/data/datasets_preproc/conquer/", source_info$rds_name, ".rds")
  if (!file.exists(rds_location)) {
    download.file(paste0("http://imlspenticton.uzh.ch/robinson_lab/conquer/data-mae/", source_info$rds_name, ".rds"), rds_location, method="libcurl") # libcurl muuuuuuuuuch faster, usually
  }
  data <- read_rds(rds_location)

  counts <- SummarizedExperiment::assay(data[["gene"]], "count") %>% t

  # filter genes
  gene_info <- SummarizedExperiment::rowData(data[["gene"]]) %>% as.data.frame()
  gene_info <- gene_info %>% filter(genome != "ERCC")
  counts <- t(counts[, gene_info$gene]) %>% rowsum(gene_info$symbol) %>% t
  gene_info <- gene_info %>% slice(match(symbol, colnames(counts)))
  gene_info$id <- gene_info$symbol

  gene_ids <- gene_info$id

  # filter cells
  cell_info <- SummarizedExperiment::colData(data) %>% as.data.frame() %>% tibble::rownames_to_column("cell_id")
  if(is.character(source_info$milestone_source)) {
    cell_info$milestone_id <- cell_info[[source_info$milestone_source]]
  } else {
    cell_info$milestone_id <- source_info$milestone_source(cell_info)
  }

  milestone_network <- source_info$milestone_network
  milestone_ids <- unique(c(milestone_network$from, milestone_network$to))
  milestone_percentages <- cell_info %>% mutate(percentage=1) %>% select(cell_id, milestone_id, percentage)

  cell_info <- cell_info %>% filter(milestone_id %in% milestone_ids)

  counts <- counts[cell_info$cell_id, ]
  cell_info$num_genes_expressed <- counts %>% apply(1, function(x) sum(x>0))
  # cell_info$num_reads <- counts %>% apply(1, function(x) sum(x>0))
  cell_info <- cell_info %>% filter(num_genes_expressed > 0)
  counts <- counts[cell_info$cell_id, ]

  cell_grouping <- cell_info %>% select(cell_id, milestone_id) %>% rename(group_id = milestone_id)

  cell_ids <- rownames(counts)

  # counts and expression
  expression <- log2(counts + 1)

  # info
  info <- list(id=source_info$id)

  dataset <- lst(gene_info, cell_info, cell_grouping, cell_ids, gene_ids, counts, expression, milestone_network, milestone_ids, milestone_percentages, info)

  dynalysis::save_dataset(dataset)
}
