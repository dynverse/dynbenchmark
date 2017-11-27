rm(list=ls())
library(tidyverse)
library(dynalysis)

requireNamespace("MultiAssayExperiment")

addcols <- function(x) x %>% mutate(length = 1, directed = TRUE)

get_rds <- function(rds_name) {
  rds_location <- dataset_preproc_file(pritt("{rds_name}.rds"))
  if (!file.exists(rds_location)) {
    rds_remote_location <- pritt("http://imlspenticton.uzh.ch/robinson_lab/conquer/data-mae/{rds_name}.rds")
    download.file(rds_remote_location, rds_location, method = "libcurl")
  }
  read_rds(rds_location)
}

conquer_infos <- list(
  list(
    id = "cell_cycle_buettner",
    rds_name = "EMTAB2805",
    milestone_source = "cell_cycle_stage",
    milestone_network = tribble(
      ~from, ~to,
      "G1", "S",
      "S", "G2M",
      "G2M", "G1"
    ) %>% addcols,
    organism = "??",
    source_id = "conquer"
  ),
  list(
    id = "human_embryos_petropoulos",
    rds_name = "EMTAB3929",
    milestone_source = "Characteristics.developmental.stage.",
    milestone_network = tribble(
      ~from, ~to,
      "embryonic day 3", "embryonic day 4",
      "embryonic day 4", "embryonic day 5",
      "embryonic day 5", "embryonic day 6",
      "embryonic day 6", "embryonic day 7"
    ) %>% addcols,
    organism = "human",
    source_id = "conquer"
  ),
  list(
    id = "NKT_differentiation_engel",
    rds_name = "GSE74596",
    milestone_source = "characteristics_ch1.5",
    milestone_network = tribble(
      ~from, ~to,
      "vα14 inkt thymocyte subset: NKT0", "vα14 inkt thymocyte subset: NKT1",
      "vα14 inkt thymocyte subset: NKT0", "vα14 inkt thymocyte subset: NKT17",
      "vα14 inkt thymocyte subset: NKT0", "vα14 inkt thymocyte subset: NKT2"
    ) %>% addcols,
    organism = "mus_musculus",
    source_id = "conquer"
  ),
  list(
    id = "cell_cycle_leng",
    rds_name = "GSE64016",
    milestone_source = function(cell_info) gsub("(.*)_.*", "\\1", as.character(cell_info$title)),
    milestone_network = tribble(
      ~from, ~to,
      "G1", "S",
      "S", "G2",
      "G2", "G1"
    ) %>% addcols
  ),
  list(
    id = "mesoderm_development_loh",
    rds_name = "SRP073808",
    milestone_source = "LibraryName",
    milestone_network = tribble( # see http://ars.els-cdn.com/content/image/1-s2.0-S0092867416307401-fx1_lrg.jpg
      ~from, ~to,
      "H7hESC", "H7_derived_APS",
      "H7hESC", "H7_derived_MPS",
      "H7_derived_APS", "H7_derived_DLL1pPXM",
      "H7_derived_DLL1pPXM", "H7_derived_ESMT",
      "H7_derived_ESMT", "H7_derived_Sclrtm",
      "H7_derived_ESMT", "H7_derived_D5CntrlDrmmtm",
      "H7_derived_MPS", "H7_derived_D2LtM",
      "H7_derived_D2LtM", "H7_derived_D3GARPpCrdcM",
      "H7_derived_D2LtM", "H7_dreived_D2.25_Smtmrs"
    ) %>% addcols
  ),
  list(
    id = "myoblast_differentiation_trapnell",
    rds_name = c("GSE52529-GPL11154", "GSE52529-GPL16791"),
    paper = "https://www.nature.com/nbt/journal/v32/n4/full/nbt.2859.html",
    milestone_source = function(cell_info) gsub("Cell (T\\d*)_.*", "\\1", as.character(cell_info$title)),
    milestone_network = tribble(
      ~from, ~to,
      "T0", "T24",
      "T24", "T48",
      "T48", "T72"
    ) %>% addcols,
    organism = "mus_musculus",
    source_id = "conquer"
  ),
  list(
    id = "germline_human_female_guo",
    rds_name = "GSE63818-GPL16791",
    milestone_network = tribble(
      ~from, ~to, ~length, ~directed,
      "F4W", "F8W", 4, TRUE,
      "F8W", "F10W", 2, TRUE,
      "F10W", "F11W", 1, TRUE,
      "F11W", "F17W", 6, TRUE
    ),
    milestone_source = function(cell_info) gsub("(.).*_([0-9]*W)_.*", "\\1\\2", cell_info$title)
  ),
  list(
    id = "germline_human_male_guo",
    rds_name = "GSE63818-GPL16791",
    milestone_network = tribble(
      ~from, ~to, ~length, ~directed,
      "M4W", "M7W", 3, TRUE,
      "M7W", "M10W", 3, TRUE,
      "M10W", "M11W", 1, TRUE,
      "M11W", "M19W", 8, TRUE
    ),
    milestone_source = function(cell_info) gsub("(.).*_([0-9]*W)_.*", "\\1\\2", cell_info$title)
  ),
  list(
    id = "germline_human_both_guo",
    rds_name = "GSE63818-GPL16791",
    milestone_network = tribble(
      ~from, ~to, ~length, ~directed,
      "4W", "7W", 3, TRUE,
      "7W", "8W", 1, TRUE,
      "8W", "10W", 2, TRUE,
      "10W", "11W", 1, TRUE,
      "11W", "F17W", 6, TRUE,
      "11W", "M19W", 8, TRUE
    ),
    milestone_source = function(cell_info) {
      src <- gsub("(.).*_([0-9]*W)_.*", "\\1\\2", cell_info$title)
      ifelse(src %in% c("F17W", "M19W"), src, gsub("[FM]", "", src))
    }
  )
)  %>% {set_names(., map(., "id"))}


for (id in names(conquer_infos)) {
  cat(pritt("Processing {id}"), sep = "\n")
  source_info <- conquer_infos[[id]]

  dataset_preprocessing("real", id)

  datas <- lapply(source_info$rds_name, get_rds)

  tran_counts <- do.call(rbind, lapply(datas, function(data) SummarizedExperiment::assay(data[["gene"]], "count") %>% t))

  # filter genes
  transcript_info <- datas %>% map_df(~ SummarizedExperiment::rowData(.[["gene"]]) %>% as.data.frame) %>%
    unique() %>%
    filter(genome != "ERCC")
  counts <- tran_counts[, transcript_info$gene] %>% t %>% rowsum(transcript_info$symbol) %>% t
  feature_info <- transcript_info %>%
    group_by(symbol) %>%
    summarise(ensembl_ids = paste(gene, collapse = ",")) %>%
    slice(match(symbol, colnames(counts))) %>%
    mutate_all(as.character) %>%
    select(feature_id = symbol, everything())

  # filter cells
  cell_info <- bind_rows(lapply(datas, function(data) {
    SummarizedExperiment::colData(data) %>%
      as.data.frame() %>%
      tibble::rownames_to_column("cell_id") %>%
      mutate_if(is.factor, as.character)
  }))

  if (is.character(source_info$milestone_source)) {
    cell_info$milestone_id <- cell_info[[source_info$milestone_source]]
  } else {
    cell_info$milestone_id <- source_info$milestone_source(cell_info)
  }

  milestone_network <- source_info$milestone_network
  milestone_ids <- unique(c(milestone_network$from, milestone_network$to))
  milestone_percentages <- cell_info %>%
    mutate(percentage = 1) %>%
    select(cell_id, milestone_id, percentage)

  cell_info <- cell_info %>% filter(milestone_id %in% milestone_ids)

  counts <- counts[cell_info$cell_id, ]
  cell_info$num_genes_expressed <- counts %>% apply(1, function(x) sum(x>0))
  cell_info <- cell_info %>% filter(num_genes_expressed > 0)
  counts <- counts[cell_info$cell_id, ]

  cell_grouping <- cell_info %>% select(cell_id, milestone_id) %>% rename(group_id = milestone_id)

  cell_ids <- rownames(counts)

  # TODO: use dynutils normalisation
  expression <- log2(counts + 1)

  dataset <- wrap_ti_task_data(
    ti_type = "real",
    id = datasetpreproc_getid(),
    counts = counts,
    expression = expression,
    cell_ids = cell_ids,
    milestone_ids = milestone_ids,
    milestone_network = milestone_network,
    milestone_percentages = milestone_percentages,
    cell_grouping = cell_grouping,
    cell_info = cell_info,
    feature_info = feature_info
  )

  save_dataset(dataset)
}
