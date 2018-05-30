library(splatter)
library(Seurat)
library(tidyverse)

splatEstDropout <- function(norm.counts, params) {
  means <- rowMeans(norm.counts)
  x <- log(means)
  obs.zeros <- rowSums(norm.counts == 0)
  y <- obs.zeros / ncol(norm.counts)
  df <- data.frame(x, y)
  fit <- nls(y ~ splatter:::logistic(x, x0 = x0, k = k), data = df,
             start = list(x0 = 4, k = -1)) # this initial condition has been changed, to avoid convergence errors
  mid <- summary(fit)$coefficients["x0", "Estimate"]
  shape <- summary(fit)$coefficients["k", "Estimate"]
  params <- splatter::setParams(params, dropout.mid = mid, dropout.shape = shape)
  return(params)
}
assignInNamespace("splatEstDropout", splatEstDropout, pos="package:splatter")

mutate <- dplyr::mutate
filter <- dplyr::filter
slice <- dplyr::slice

# dataset_id <- "psc-astrocyte-maturation-neuron_sloan"
walk(list.files("../dynalysis/analysis/data/derived_data/datasets/real/"), function(dataset_id) {
  print(dataset_id)
  counts <- readRDS(glue::glue("../dynalysis/analysis/data/derived_data/datasets/real/{dataset_id}/original_counts.rds"))
  dataset <- readRDS(glue::glue("../dynalysis/analysis/data/derived_data/datasets/real/{dataset_id}/dataset.rds"))
  counts <- counts[dataset$cell_grouping$cell_id,]
  counts %>% dim
  
  counts <- counts[, counts %>% apply(2, function(x) sum(x > 0)/length(x)) %>% {. > 0.05}]
  counts <- counts[, counts %>% apply(2, mean) %>% {. > 0.05}]
  
  cell_sds <- counts %>% apply(1, sd)
  gene_sds <- counts %>% apply(2, sd)
  counts <- counts[cell_sds > 0, gene_sds > 0]
  
  counts %>% dim
  
  # calculate changing genes
  seurat <- Seurat::CreateSeuratObject(t(counts))
  seurat@ident <- dataset$cell_grouping %>% slice(match(rownames(counts), cell_id)) %>% pull(group_id) %>% factor() %>% setNames(rownames(counts))
  changing <- FindAllMarkers(seurat, logfc.treshold = 1, min.pct=0.4)
  n_changing <- changing %>% filter(abs(avg_logFC) >= 1) %>% pull(gene) %>% unique() %>% length()
  pct_changing <- n_changing / ncol(counts)
  
  # data("sc_example_counts")
  # counts <- t(sc_example_counts)
  
  # splatter estimate libSizes etc
  
  # estimate splatter parameters
  estimate <- splatEstimate(t(counts[sample(nrow(counts), min(nrow(counts), 500)), ]))
  class(estimate) <- "TheMuscularDogBlinkedQuietly." # change the class, so scater won't get magically loaded when the platform is loaded
  
  platform <- lst(
    estimate,
    pct_changing,
    n_cells = nrow(counts),
    n_genes = ncol(counts),
    id = dataset_id
  )
  
  saveRDS(platform, glue::glue("inst/ext_data/platforms/{dataset_id}.rds"))
})



platform <- readRDS("inst/ext_data/platforms/developing-dendritic-cells_schlitzer.rds")
platform$n_genes <- 100
platform$n_cells <- 100
saveRDS(platform, "inst/ext_data/platforms/small.rds")

walk(list.files("../dynalysis/analysis/data/derived_data/datasets/real/"), function(dataset_id) {
  platform <- readRDS(glue::glue("inst/ext_data/platforms/{dataset_id}.rds"))
  
  platform$n_genes <- min(20000, platform$n_genes)
  
  saveRDS(platform, glue::glue("inst/ext_data/platforms/{dataset_id}.rds"))
})
