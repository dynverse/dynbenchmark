rm(list=ls())

files <- c(
  "analysis/data/datasets_preproc/GSE70240_Gmp.txt.gz" = "https://www.ncbi.nlm.nih.gov/geo/download/?acc=GSE70240&format=file&file=GSE70240%5FGmp%2Etxt%2Egz",
  "analysis/data/datasets_preproc/GSE70243_LK.CD34+.txt.gz" = "https://www.ncbi.nlm.nih.gov/geo/download/?acc=GSE70243&format=file&file=GSE70243%5FLK%2ECD34%2B%2Etxt%2Egz",
  "analysis/data/datasets_preproc/GSE70244_Lsk.txt.gz" = "https://www.ncbi.nlm.nih.gov/geo/download/?acc=GSE70244&format=file&file=GSE70244%5FLsk%2Etxt%2Egz",
  "analysis/data/datasets_preproc/GSE70236_Cmp.txt.gz" = "https://www.ncbi.nlm.nih.gov/geo/download/?acc=GSE70236&format=file&file=GSE70236%5FCmp%2Etxt%2Egz"
)

walk2(files, names(files), function(txt_web_location, txt_location) {
  if (!file.exists(txt_location)) {
    download.file(paste0(txt_web_location), txt_location, method="libcurl") # libcurl muuuuuuuuuch faster, usually
  }
})

allexpression <- map(names(files), ~read_tsv(.) %>% tibble::column_to_rownames("uid") %>% as.matrix) %>% do.call(cbind, .) %>% t

txt_web_location <- "https://www.nature.com/nature/journal/v537/n7622/source_data/nature19348-f1.xlsx"
txt_location <- "analysis/data/datasets_preproc/nature19348-f1.xlsx"

if (!file.exists(txt_location)) {
  download.file(paste0(txt_web_location), txt_location, method="libcurl") # libcurl muuuuuuuuuch faster, usually
}

small_expression <- readxl::read_xlsx(txt_location, 1)
subclusters <- small_expression[1, -c(1, 2)] %>% as.list() %>% unlist()
subclusters_mapper <- c("HSCP-1", "HSCP-2", "Meg", "Eryth", "Multi-Lin", "MDP", "Mono", "Gran", "Myelocyte")
subclusters <- subclusters_mapper[subclusters] %>% set_names(names(subclusters))

allcell_info <- tibble(
  cell_id=names(subclusters) %>% gsub(".*:(.*)", "\\1", .),
  cluster = subclusters,
  gate = names(subclusters) %>% gsub("(.*):.*", "\\1", .)
)

settings <- list(
  list(
    id = "hematopoiesis_olsson_clusters",
    milestone_source = "cluster",
    milestone_network = tribble(
      ~from, ~to,
      "HSCP-1", "HSCP-2",
      "HSCP-2", "Multi-Lin",
      "Multi-Lin", "MDP",
      "Multi-Lin", "Eryth",
      "Multi-Lin", "Meg",
      "MDP", "Gran",
      "MDP", "Mono",
      "Gran", "Myelocyte"
    )
  ),
  list(
    id = "hematopoiesis_olsson_gates",
    milestone_source = "gate",
    milestone_network = tribble(
      ~from, ~to,
      "Lsk", "Cmp",
      "Cmp", "Gmp"
    )
  )
)

for (setting in settings) {
  info <- list(id=setting$id)
  milestone_network <- setting$milestone_network
  cell_info <- allcell_info
  cell_info$milestone_id <- cell_info[[setting$milestone_source]]

  milestone_ids <- unique(c(milestone_network$from, milestone_network$to))

  cell_info <- cell_info %>% filter(milestone_id %in% milestone_ids)
  expression <- allexpression[cell_info$cell_id, ]
  cell_ids <- cell_info$cell_id

  cell_grouping <- cell_info %>% select(cell_id, milestone_id) %>% rename(group_id = milestone_id)
  milestone_percentages <- cell_grouping %>% rename(milestone_id=group_id) %>% mutate(percentage=1)

  gene_info <- tibble(id=colnames(expression))
  gene_ids <- gene_info$id

  dataset <- lst(gene_info, cell_info, cell_grouping, cell_ids, gene_ids, expression, milestone_network, milestone_ids, milestone_percentages, info)

  dynalysis:::save_dataset(dataset)
}
