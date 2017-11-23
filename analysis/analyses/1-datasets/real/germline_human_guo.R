library(tidyverse)

requireNamespace("MultiAssayExperiment")
source("data-raw/conquer_infos.R")

source_info <- conquer_infos$germline_human_guo

rds_location <- paste0("analysis/data/datasets_preproc/conquer/", source_info$rds_name, ".rds")
if (!file.exists(rds_location)) {
  download.file(paste0("http://imlspenticton.uzh.ch/robinson_lab/conquer/data-mae/", source_info$rds_name, ".rds"), rds_location, method="libcurl") # libcurl muuuuuuuuuch faster, usually
}
data <- read_rds(rds_location)

allcounts <- SummarizedExperiment::assay(data[["gene"]], "count") %>% t
rownames(allcounts) <- SummarizedExperiment::colData(data) %>% as.data.frame() %>% pull(title) %>% as.character()

# filter genes
gene_info <- SummarizedExperiment::rowData(data[["gene"]]) %>% as.data.frame()
gene_info <- gene_info %>% filter(genome != "ERCC")
allcounts <- t(allcounts[, gene_info$gene]) %>% rowsum(gene_info$symbol) %>% t
gene_info <- gene_info %>% slice(match(symbol, colnames(allcounts)))
gene_info$id <- gene_info$symbol

gene_ids <- gene_info$id

# filter cells
allcell_info <- SummarizedExperiment::colData(data) %>% as.data.frame() %>% mutate(cell_id=as.character(title)) %>% select(cell_id)
splitted <- strsplit(allcell_info$cell_id, "_")
allcell_info <- allcell_info %>% mutate(gender=map(splitted, 1), type=map(splitted, 2), weeks=as.integer(map(splitted, ~gsub("(.*)W", "\\1", .[[3]]))))
allcell_info$genderweek <- paste0(allcell_info$gender, allcell_info$week)
allcell_info$genderweek_subset <- paste0(ifelse(allcell_info$week < 17, "", allcell_info$gender), allcell_info$week)

milestone_order_to_network <- function(order) {
  tibble(from=order[-length(order)], to=order[-1])
}

settings <- list(
  list(
    milestone_network = milestone_order_to_network(allcell_info %>% arrange(weeks) %>% filter(gender == "F") %>% pull(genderweek) %>% unique()) %>% mutate(length=as.numeric(substring(to, 2))-as.numeric(substring(from, 2))),
    id = "germline_human_female_guo",
    milestone_source = "genderweek"
  ),
  list(
    milestone_network = milestone_order_to_network(allcell_info %>% arrange(weeks) %>% filter(gender == "M") %>% pull(genderweek) %>% unique()) %>% mutate(length=as.numeric(substring(to, 2))-as.numeric(substring(from, 2))),
    id = "germline_human_male_guo",
    milestone_source = "genderweek"
  ),
  list(
    milestone_network = tribble(
      ~from, ~to, ~length,
      "4", "7", 3,
      "7", "8", 1,
      "8", "10", 2,
      "10", "11", 1,
      "11", "F17", 6,
      "11", "M19", 8
    ),
    id = "germline_human_both_guo",
    milestone_source = "genderweek_subset"
  )
)

for (setting in settings) {
  milestone_network <- setting$milestone_network
  info <- list(id=setting$id)

  milestone_ids <- unique(c(milestone_network$from, milestone_network$to))
  cell_info <- allcell_info
  cell_info$milestone_id <- cell_info[[setting$milestone_source]]
  milestone_percentages <- cell_info %>% mutate(percentage=1) %>% select(cell_id, milestone_id, percentage)

  cell_info <- cell_info %>% filter(milestone_id %in% milestone_ids)

  counts <- allcounts[cell_info$cell_id, ]
  cell_info$num_genes_expressed <- counts %>% apply(1, function(x) sum(x>0))
  # cell_info$num_reads <- counts %>% apply(1, function(x) sum(x>0))
  cell_info <- cell_info %>% filter(num_genes_expressed > 0)
  counts <- counts[cell_info$cell_id, ]

  cell_grouping <- cell_info %>% select(cell_id, milestone_id) %>% rename(group_id = milestone_id)

  cell_ids <- rownames(counts)

  # counts and expression
  expression <- log2(counts + 1)

  dataset <- lst(gene_info, cell_info, cell_grouping, cell_ids, gene_ids, counts, expression, milestone_network, milestone_ids, milestone_percentages, info)

  dynalysis:::save_dataset(dataset)
}
