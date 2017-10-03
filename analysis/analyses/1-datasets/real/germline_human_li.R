rm(list=ls())

library(tidyverse)

preproc_folder <- "analysis/data/datasets_preproc/GSE86146/"
GSMs <- list.files(preproc_folder) %>% {.[startsWith(., "GSM")]}
samples <- map(GSMs, ~read_tsv(paste0(preproc_folder, .)))
genes <- samples[[1]]$Gene
samples <- map(samples, ~select(., -Gene))
allcounts <- bind_cols(samples) %>% as.matrix() %>% magrittr::set_rownames(genes) %>% t

allcell_info <- readxl::read_xlsx("analysis/data/datasets_preproc/GSE86146/mmc2.xlsx", sheet=3) %>%
  rename(cell_id = Cell, cluster=Cluster) %>%
  filter(cell_id %in% rownames(allcounts)) %>%
  mutate(
    week = as.numeric(gsub("[FM]_(.+)W_.*", "\\1", cell_id)),
    gender = gsub("([FM]).*", "\\1", cell_id),
    type = gsub(".*_(Soma|FGC).*", "\\1", cluster),
    weekgendertype = paste0(gender, "#", week, "#", type)
  )

milestone_order_to_network <- function(order) {
  tibble(from=order[-length(order)], to=order[-1])
}

settings <- list(
  list(
    id="germline_human_male_li",
    milestone_network = tibble(from=c("Male_FGC#1", "Male_FGC#2"), to=c("Male_FGC#2", "Male_FGC#3")),
    milestone_source = "cluster"
  ),
  list(
    id="germline_human_female_li",
    milestone_network = tibble(from=c("Female_FGC#1", "Female_FGC#2", "Female_FGC#3"), to=c("Female_FGC#2", "Female_FGC#3", "Female_FGC#4")),
    milestone_source = "cluster"
  ),
  list(
    id="germline_human_male_weeks_li",
    milestone_network = milestone_order_to_network(allcell_info %>% filter(gender == "M" & type == "FGC") %>% arrange(week) %>% pull(weekgendertype) %>% unique()),
    milestone_source = "weekgendertype"
  ),
  list(
    id="germline_human_female_weeks_li",
    milestone_network = milestone_order_to_network(allcell_info %>% filter(gender == "F" & type == "FGC") %>% arrange(week) %>% pull(weekgendertype) %>% unique()),
    milestone_source = "weekgendertype"
  )
)

for (setting in settings) {
  source_info <- list(source_id="GEO")

  milestone_network <- setting$milestone_network
  milestone_ids <- unique(c(milestone_network$from, milestone_network$to))

  cell_info <- allcell_info
  cell_info$milestone_id <- cell_info[[setting$milestone_source]]
  cell_info <- cell_info %>% filter(milestone_id %in% milestone_ids)
  counts <- allcounts[cell_info$cell_id, ]
  cell_ids <- cell_info$cell_id

  cell_grouping <- cell_info %>% select(cell_id, milestone_id) %>% rename(group_id = milestone_id)
  milestone_percentages <- cell_grouping %>% rename(milestone_id=group_id) %>% mutate(percentage=1)

  gene_info <- tibble(id=colnames(counts))
  gene_ids <- gene_info$id

  expression <- log2(counts + 1)

  dataset <- lst(gene_info, cell_info, cell_grouping, cell_ids, gene_ids, counts, expression, milestone_network, milestone_ids, milestone_percentages, info=list(id=setting$id))

  dynalysis:::save_dataset(dataset)
}
