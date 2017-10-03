rm(list=ls())
library(tidyverse)

data1 <- readRDS("analysis/data/datasets_preproc/conquer/GSE52529-GPL11154.rds")
data2 <- readRDS("analysis/data/datasets_preproc/conquer/GSE52529-GPL16791.rds")

requireNamespace("MultiAssayExperiment")

source("data-raw/helpers/conquer_infos.R")
source_info <- conquer_infos$myoblast_differentiation_trapnell

{
  counts <- rbind(
    SummarizedExperiment::assay(data1[["gene"]], "count") %>% t,
    SummarizedExperiment::assay(data2[["gene"]], "count") %>% t
  )

  # filter genes
  gene_info <- bind_rows(
    SummarizedExperiment::rowData(data1[["gene"]]) %>% as.data.frame(),
    SummarizedExperiment::rowData(data2[["gene"]]) %>% as.data.frame()
  )
  gene_info <- gene_info %>% filter(genome != "ERCC")
  counts <- t(counts[, gene_info$gene]) %>% rowsum(gene_info$symbol) %>% t
  gene_info <- gene_info %>% slice(match(symbol, colnames(counts)))
  gene_info$id <- gene_info$symbol

  gene_ids <- gene_info$id

  # filter cells
  cell_info <- bind_rows(
    SummarizedExperiment::colData(data1) %>% as.data.frame() %>% tibble::rownames_to_column("cell_id"),
    SummarizedExperiment::colData(data2) %>% as.data.frame() %>% tibble::rownames_to_column("cell_id")
  )

  cell_info$milestone_id <- source_info$milestone_source(cell_info)

  milestone_network <- source_info$milestone_network
  milestone_ids <- unique(c(milestone_network$from, milestone_network$to))
  cell_grouping <- cell_info %>% select(cell_id, milestone_id) %>% rename(group_id = milestone_id)
  milestone_percentages <- cell_grouping %>% rename(milestone_id=group_id) %>% mutate(percentage=1)

  cell_info <- cell_info %>% filter(milestone_id %in% milestone_ids)

  counts <- counts[cell_info$cell_id, ]

  cell_ids <- rownames(counts)

  # counts and expression
  expression <- log2(counts + 1)

  # info
  info <- list(id=source_info$id)

  dataset <- lst(gene_info, cell_info, cell_grouping, cell_ids, gene_ids, counts, expression, milestone_network, milestone_ids, milestone_percentages, info)

  dynalysis:::save_dataset(dataset)
}

