# almost impossible to retrieve the trajectory from the data, even when using the set of genes provided by the authors for ordering

library(dynbenchmark)
library(tidyverse)
library(GEOquery)
library(Biobase)
options('download.file.method.GEOquery'='curl')

id <- "real/ESC-differentiation_zhao"
dataset_preprocessing(id)

# counts & feature_info
counts_zip <- download_dataset_source_file(
  "GSE114952_AGG.tar.gz",
  "https://www.ncbi.nlm.nih.gov/geo/download/?acc=GSE114952&format=file&file=GSE114952%5FAGG%2Etar%2Egz"
)

system(paste0("tar -xvzf ", counts_zip, " -C ", dataset_source_file()))

load(dataset_source_file("matrix.RData"))
counts_all <- t(matrix)
rm(matrix)

counts_all <- counts_all[sample(rownames(counts_all), 3000), ]

feature_info_all <- read_csv(dataset_source_file("fData.csv")) %>% mutate(feature_id = gene_short_name)

colnames(counts_all) <- feature_info_all$gene_short_name

counts_all <- counts_all[, feature_info_all %>% filter(use_for_ordering) %>% pull(gene_short_name)]
feature_info_all <- feature_info_all %>% slice(match(colnames(counts_all), feature_id))

# diff exp file
diffexp_file <- download_dataset_source_file(
  "1-s2.0-S1934590918302807-mmc4.xlsx",
  "https://ars.els-cdn.com/content/image/1-s2.0-S1934590918302807-mmc4.xlsx"
)

diffexp <- readxl::read_xlsx(diffexp_file) %>% filter(Genes %in% colnames(counts_all))
diffexp_success <- diffexp %>% filter(log2fc > 1) %>% pull(Genes)
diffexp_failed <- diffexp %>% filter(log2fc < -1) %>% pull(Genes)
diffexp_success_mean <- mean(counts_all[, diffexp_success])
diffexp_failed_mean <- mean(counts_all[, diffexp_failed])

# cell info
cell_info_all <- read_csv(dataset_source_file("pData.csv")) %>%
  rename(cell_id = Barcode, time_id = Batch, cluster_id = Cluster) %>%
  slice(match(rownames(counts_all), cell_id))

clusters <- cell_info_all %>% group_by(cluster_id) %>% filter(n() > 10) %>% summarise(cells = list(cell_id)) %>% deframe()

clusters_failed_vs_success <- map(clusters, function(cell_ids) {
  print("~")
  diffexp_success_mean_cluster <- mean(counts_all[cell_ids, diffexp_success])
  diffexp_failed_mean_cluster <- mean(counts_all[cell_ids, diffexp_failed])

  log(diffexp_success_mean_cluster) - log(diffexp_failed_mean_cluster)
}) %>% set_names(names(clusters))


cell_info_all$failed_vs_success <- clusters_failed_vs_success[cell_info_all$cluster_id] %>%
  map_dbl(~ifelse(is.null(.), 0, .))

cell_info_all$success <- cell_info_all$failed_vs_success > 0


dimred <- dyndimred::dimred_pca(log2(as.matrix(counts_all) + 1))
cells <- dimred %>% as.data.frame() %>% bind_cols(cell_info_all)

ggplot(cells) + geom_point(aes(comp_1, comp_2, color = time_id))


milestone_network <- tribble(
  ~from, ~to,
  "MEF", "SI5D",
  "SI5D", "SI12D",
  "SI12D", "XEN",
  "XEN", "SII8D_success",
  "SII8D_success", "SII12D_success",
  "SII12D_success", "SIII3D_success",
  "SIII3D_success", "SIII6D_success",
  "SIII6D_success", "SIII8D_success",
  "SIII8D_success", "SIII10D_success",
  "SIII10D_success", "SIII15D_success",
  "SIII15D_success", "SIII21D_success",
  "SII8D_failed", "SII12D_failed",
  "SII12D_failed", "SIII3D_failed",
  "SIII3D_failed", "SIII6D_failed",
  "SIII6D_failed", "SIII8D_failed",
  "SIII8D_failed", "SIII10D_failed",
  "SIII10D_failed", "SIII15D_failed",
  "SIII15D_failed", "SIII21D_failed"
)
