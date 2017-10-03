txt_web_location <- "https://www.ncbi.nlm.nih.gov/geo/download/?acc=GSE81275&format=file&file=GSE81275%5Fesc1%2EexpMatrix%2Etxt%2Egz"
txt_location <- "data/preproc/GSE81275_esc1.expMatrix.txt.gz"

if (!file.exists(txt_location)) {
  download.file(paste0(txt_web_location), txt_location, method="libcurl") # libcurl muuuuuuuuuch faster, usualy
}

counts <- read_tsv(txt_location) %>% na.omit() %>% column_to_rownames("Gene") %>% as.matrix() %>% t
# this produces some warnings due to problems with line endings

counts <- counts[counts %>% apply(1, function(x) sum(x>0)) %>% {.>2000}, ]

expression <- log2(counts + 1)

# remove first component as described in paper, otherwise enormous batch effects
res <- prcomp(expression, scale.=FALSE, center=FALSE)
trunc <- res$x[,-c(1:10)] %*% t(res$rotation[,-c(1:10)])
if(res$scale != FALSE){
  trunc <- scale(trunc, center = FALSE , scale=1/res$scale)
}
if(res$center != FALSE){
  trunc <- scale(trunc, center = -1 * res$center, scale=FALSE)
}
dimnames(trunc) <- dimnames(expression)
new_expression <- trunc

source("../dynmodular/dimred_wrappers.R")
space <- dimred_mds(new_expression[apply(new_expression, 1, sd) > 0, ])
ggplot(space %>% as.data.frame() %>% rownames_to_column("cell_id") %>% left_join(cell_info)) + geom_point(aes(Comp1, Comp2, color=milestone_id))


cell_info <- tibble(cell_id = rownames(new_expression), milestone_id = gsub("esc1_(\\d*h)_.*_rsem", "\\1", rownames(new_expression)))

info <- list(organism = "mouse", genenames = "ensemble_gene", id="motor_neuron_programming_velasco", normalization = "scaled_log2")

source_info <- list(source_id="GEO")

milestone_network <- tribble(
  ~from, ~to, ~length,
  "0h", "6h", 6,
  "6h", "12h", 6,
  "12h", "18h", 6,
  "18h", "24h", 6,
  "24h", "30h", 6,
  "30h", "36h", 6,
  "36h", "48h", 12
)

milestone_ids <- unique(c(milestone_network$from, milestone_network$to))

cell_info <- cell_info %>% filter(milestone_id %in% milestone_ids)
new_expression <- new_expression[cell_info$cell_id, ]
new_expression <- new_expression[apply(new_expression, 1, sd) > 0,]
cell_info <- cell_info %>% slice(match(rownames(new_expression), cell_id))

cell_ids <- cell_info$cell_id

cell_grouping <- cell_info %>% select(cell_id, milestone_id) %>% rename(group_id = milestone_id)
milestone_percentages <- cell_grouping %>% rename(milestone_id=group_id) %>% mutate(percentage=1)

gene_info <- tibble(id=colnames(new_expression))
gene_ids <- gene_info$id

dataset <- lst(gene_info, cell_info, cell_grouping, cell_ids, gene_ids, expression=new_expression, milestone_network, milestone_ids, milestone_percentages, source_info, info)


saveRDS(dataset, paste0("data/", info$id, ".rds"))
