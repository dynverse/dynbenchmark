txt_web_location <- "https://www.ncbi.nlm.nih.gov/geo/download/?acc=GSE89405&format=file&file=GSE89405%5FKakaradov%5Fsinglecell%5Fexpression%5Fmatrix%2Ecsv%2Egz"
txt_location <- "data/preproc/GSE89405_Kakaradov_singlecell_expression_matrix.csv.gz"

if (!file.exists(txt_location)) {
  download.file(paste0(txt_web_location), txt_location, method="libcurl") # libcurl muuuuuuuuuch faster, usualy
}

counts <- read_csv(txt_location)
counts <- counts %>% tibble::column_to_rownames("Unnamed: 0") %>% select(-1) %>% as.matrix()

geo <- GEOquery::getGEO("GSE89405")
cell_info <- Biobase::phenoData(geo$`GSE89405-GPL17021_series_matrix.txt.gz`) %>% as("data.frame") %>% rename(cell_id=title, milestone_id=characteristics_ch1.1) %>% select(cell_id, milestone_id) %>% mutate_all(funs(as.character))

counts <- counts[cell_info$cell_id, ]

info <- list(organism = "mouse", genenames = "symbol", technology = "fluidigm_c1", id="cd8t_differentation_kakaradov", normalization = "counts")

source_info <- list(source_id="GEO")

milestone_network = tribble(
  ~from, ~to,
  "subtype: Naive", "subtype: Div1mem",
  "subtype: Naive", "subtype: Div1te",
  "subtype: Div1te", "subtype: Day4",
  "subtype: Day4", "subtype: Day7",
  "subtype: Div1mem", "subtype: Tcm",
  "subtype: Div1mem", "subtype: Tem"
)
milestone_ids <- unique(c(milestone_network$from, milestone_network$to))

cell_info <- cell_info %>% filter(milestone_id %in% milestone_ids)
counts <- counts[cell_info$cell_id, ]
cell_ids <- cell_info$cell_id

cell_grouping <- cell_info %>% select(cell_id, milestone_id) %>% rename(group_id = milestone_id)
milestone_percentages <- cell_grouping %>% rename(milestone_id=group_id) %>% mutate(percentage=1)

gene_info <- tibble(id=colnames(counts))
gene_ids <- gene_info$id

expression <- log2(counts + 1)


dataset <- lst(gene_info, cell_info, cell_grouping, cell_ids, gene_ids, counts, expression, milestone_network, milestone_ids, milestone_percentages, source_info, info)


saveRDS(dataset, paste0("data/", info$id, ".rds"))
