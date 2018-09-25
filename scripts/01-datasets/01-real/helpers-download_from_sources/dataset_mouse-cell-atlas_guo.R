# https://figshare.com/s/865e694ad06d5857db4b

library(tidyverse)
library(dynbenchmark)

dataset_preprocessing("real/silver/mouse-cell-atlas_guo")

# get settings
source(paste0(dynbenchmark::get_dynbenchmark_folder(), "/scripts/01-datasets/01-real/helpers-download_from_sources/helper_mouse-cell-atlas_settings.R"))

# cell info
cell_assignments_file <- download_dataset_source_file("MCA_CellAssignments.csv", "https://ndownloader.figshare.com/files/11083451?private_link=865e694ad06d5857db4b")
all_cell_info <- read_csv(cell_assignments_file) %>%
  rename(cell_id = Cell.name, cluster_id = ClusterID, tissue = Tissue, batch = Batch, barcode = Cell.Barcode, group_id = Annotation) %>%
  mutate(cell_type = gsub("([^\\(_]*).*", "\\1", group_id))

all_cell_info$cell_id <- all_cell_info$cell_id %>% str_replace_all("FetalFemaleGonad", "Female(fetal)Gonad") # fix for fetal female gonad names
all_cell_info$cell_id <- all_cell_info$cell_id %>% str_replace_all("NeonatalBrain", "NeontalBrain") # fix for neonatal brain names

# counts
counts_zip <- download_dataset_source_file("MCA_BatchRemove_dge.zip", "https://ndownloader.figshare.com/files/10756795?private_link=865e694ad06d5857db4b")
system(paste0("unzip -o ", counts_zip, " -d ", dataset_source_file()))

counts_files <- list.files(dataset_source_file("rmbatch_dge"), full.names = TRUE)
counts_files_unzipped <- map(counts_files, function(counts_file) {  # fix top-left corner of counts files
  counts_file_unzipped <- gsub("\\.gz", "", counts_file)
  if (!file.exists(counts_file_unzipped)) {
    read_lines(counts_file) %>% {.[[1]] <- paste0("\"gene\" ", .[[1]]); .} %>% write_lines(counts_file_unzipped)
  }
  counts_file_unzipped
})

counts_files_processed <- map_chr(counts_files_unzipped, function(counts_file_unzipped) {
  print(counts_file_unzipped)
  counts_file_processed <- gsub("\\.txt", "\\.rds", counts_file_unzipped)

  if (!file.exists(counts_file_processed)) {
    counts <- read_delim(counts_file_unzipped, " ")
    counts <- counts %>% as.data.frame() %>% column_to_rownames("gene") %>% as.matrix() %>% t

    counts %>% write_rds(counts_file_processed)
  }
  counts_file_processed
})

# map count files to each cell in cell info
cell_id_mapping <- map_df(unique(counts_files_processed), function(counts_file_processed) {
  print(counts_file_processed)
  cell_ids <- rownames(read_rds(counts_file_processed))
  tibble(
    cell_id = cell_ids,
    counts_file = counts_file_processed
  )
})

all_cell_info <- left_join(all_cell_info, cell_id_mapping, "cell_id")
all_cell_info <- all_cell_info %>% filter(!is.na(counts_file))

# load in counts of cells
get_counts <- function(cell_info) {
  counts_split <- map(unique(cell_info$counts_file), read_rds)

  common_genes <- map(counts_split, colnames) %>% Reduce(intersect, .)

  counts <- map(counts_split, ~.[rownames(.)[rownames(.) %in% cell_info$cell_id], common_genes]) %>% do.call(rbind, .)

  counts
}

# process setting
for (setting in settings) {
  print(setting$id)

  milestone_network <- setting$milestone_network
  milestone_ids <- unique(c(milestone_network$from, milestone_network$to))

  if (!"length" %in% names(milestone_network)) milestone_network$length <- 1
  milestone_network$length[is.na(milestone_network$length)] <- TRUE
  if (!"directed" %in% names(milestone_network)) milestone_network$directed <- TRUE
  milestone_network$directed[is.na(milestone_network$directed)] <- TRUE

  cell_info <- all_cell_info %>% filter(group_id %in% milestone_ids)

  grouping <- cell_info %>% select(cell_id, group_id) %>% deframe()

  counts <- get_counts(cell_info)

  counts <- counts[, !(colnames(counts) %>% str_detect("mt-.*"))] # remove mitochondrial genes, as these were already used for filtering in the original study

  save_raw_dataset(lst(milestone_network, cell_info, grouping, counts), setting$id)
}


# traj <- load_dataset(id)
#
# dimred <- SCORPIUS::reduce_dimensionality(get_expression(traj, "counts"), dist_fun = SCORPIUS::correlation_distance, ndim = 2, num_landmarks = 100) %>% magrittr::set_colnames(c("comp_1", "comp_2"))
# plot_dimred(traj, "grouping", dimred = dimred, grouping = traj$grouping)
# plot_dimred(traj, "grouping", dimred = dyndimred::dimred_mds, grouping = traj$grouping, expression_source = "expression")
# plot_dimred(traj, "grouping", dimred = dyndimred::dimred_umap, grouping = traj$grouping, expression_source = "expression")
# plot_dimred(traj, "grouping", dimred = dyndimred::dimred_umap, grouping = traj$grouping, expression_source = "counts")
#
# plot_dimred(traj, "grouping", dimred = dyndimred::dimred_umap(get_expression(traj, "counts") %>% apply(1, function(x) x/sum(x)) %>% t, n_neighbors = 15), grouping = traj$grouping)
