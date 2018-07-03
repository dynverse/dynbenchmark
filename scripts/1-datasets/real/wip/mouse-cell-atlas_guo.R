# https://figshare.com/s/865e694ad06d5857db4b

library(tidyverse)
library(dynbenchmark)

dataset_preprocessing("real/mouse-cell-atlas_guo")

# cell info
cell_assignments_file <- download_dataset_file("MCA_CellAssignments.csv", "https://ndownloader.figshare.com/files/11083451?private_link=865e694ad06d5857db4b")
all_cell_info <- read_csv(cell_assignments_file) %>%
  rename(cell_id = Cell.name, cluster_id = ClusterID, tissue = Tissue, batch = Batch, barcode = Cell.Barcode, group_id = Annotation) %>%
  mutate(cell_type = gsub("([^\\(_]*).*", "\\1", group_id))

all_cell_info$cell_id <- all_cell_info$cell_id %>% str_replace_all("FetalFemaleGonad", "Female(fetal)Gonad") # fix for fetal female gonad names
all_cell_info$cell_id <- all_cell_info$cell_id %>% str_replace_all("NeonatalBrain", "NeontalBrain") # fix for neonatal brain names

# counts
counts_zip <- download_dataset_file("MCA_BatchRemove_dge.zip", "https://ndownloader.figshare.com/files/10756795?private_link=865e694ad06d5857db4b")
system(paste0("unzip -o ", counts_zip, " -d ", dataset_preproc_file()))

counts_files <- list.files(dataset_preproc_file("rmbatch_dge"), full.names = TRUE)
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



settings <- list(
  setting <- list(
    tissue = "thymus",
    subid = "t-cell-differentiation",
    milestone_network = tribble(
      ~from, ~to,
      # "Pre T cell(Thymus)", "DPT cell(Thymus)", # pre t cells seem to be wrongly annotated
      "DPT cell(Thymus)", "gdT cell (Thymus)",
      "DPT cell(Thymus)", "abT cell(Thymus)",
      "abT cell(Thymus)", "T cell_Ms4a4b high(Thymus)",
      "T cell_Ms4a4b high(Thymus)", "T cell_Id2 high(Thymus)"
    ),
    dynamic_process = "differentiation",
    visual_check = "ok"
  ),

  list(
    tissue = "kidney",
    subid = "bursh-border-to-s1",
    milestone_network = tribble(
      ~from, ~to, ~directed,
      "Proximal tubule brush border cell(Kidney)", "Proximal tubule cell_Cyp4a14 high(Kidney)", FALSE,
      "Proximal tubule cell_Cyp4a14 high(Kidney)", "S1 proximal tubule cells(Kidney)", FALSE
    ),
    dynamic_process = "spatial",
    visual_check = "ok"
  ),

  list(
    tissue = "kidney",
    subid = "distal-convoluted-tubule",
    milestone_network = tribble(
      ~from, ~to, ~directed,
      "Distal convoluted tubule_Pvalb high(Kidney)", "Distal convoluted tubule_S100g high(Kidney)", FALSE,
      "Distal convoluted tubule_S100g high(Kidney)", "Distal collecting duct principal cell_Hsd11b2 high(Kidney)", FALSE
    ),
    dynamic_process = "spatial",
    visual_check = "ok"
  ),

  list(
    tissue = "embryonic-mesenchyme",
    subid = "stromal-cell-cxcl14-cxcl12-axis",
    milestone_network = tribble(
      ~from, ~to, ~directed,
      "Stromal Cell_Cxcl14 high(Embryonic-Mesenchyme)", "Stromal Cell_Agtr2 high(Embryonic-Mesenchyme)", FALSE,
      "Stromal Cell_Agtr2 high(Embryonic-Mesenchyme)", "Stromal cell_Cxcl12 high(Embryonic-Mesenchyme)", FALSE
    ),
    reference_doi = c("10.1038/s41598-017-17490-z"),
    dynamic_process = "spatial",
    visual_check = "failed"
  ),

  list(
    tissue = "bone-marrow-c-kit",
    subid = "monocyte-differentiation",
    milestone_network = tribble(
      ~from, ~to,
      "Multipotent progenitor_Ctla2a high(Bone-Marrow_c-kit)", "Monocyte progenitor(Bone-Marrow_c-kit)",
      "Monocyte progenitor(Bone-Marrow_c-kit)", "Monocyte progenitor cell_Prtn3 high(Bone-Marrow_c-kit)",
      "Monocyte progenitor cell_Prtn3 high(Bone-Marrow_c-kit)", "Monocyte progenitor cell_Ctsg high(Bone-Marrow_c-kit)"
    ),
    dynamic_process = "differentiation",
    visual_check = "ok"
  ),

  list(
    tissue = "bone-marrow-c-kit",
    subid = "neutrophil-differentiation",
    milestone_network = tribble(
      ~from, ~to,
      "Multipotent progenitor_Ctla2a high(Bone-Marrow_c-kit)", "Neutrophil_Ighg1 high(Bone-Marrow_c-kit)",
      "Neutrophil_Ighg1 high(Bone-Marrow_c-kit)", "Neutrophil_Lcn2 high(Bone-Marrow_c-kit)",
      "Neutrophil_Lcn2 high(Bone-Marrow_c-kit)", "Neutrophil_Ngp high(Bone-Marrow_c-kit)"
    ),
    dynamic_process = "differentiation",
    visual_check = "ok"
  ),

  list(
    tissue = "bone-marrow-c-kit",
    subid = "hematopoiesis",
    milestone_network = tribble(
      ~from, ~to,
      "Multipotent progenitor_Ctla2a high(Bone-Marrow_c-kit)", "Monocyte progenitor(Bone-Marrow_c-kit)",
      "Monocyte progenitor(Bone-Marrow_c-kit)", "Monocyte progenitor cell_Prtn3 high(Bone-Marrow_c-kit)",
      "Monocyte progenitor cell_Prtn3 high(Bone-Marrow_c-kit)", "Monocyte progenitor cell_Ctsg high(Bone-Marrow_c-kit)",
      "Multipotent progenitor_Ctla2a high(Bone-Marrow_c-kit)", "Megakaryocyte progenitor cell(Bone-Marrow_c-kit)",
      "Multipotent progenitor_Ctla2a high(Bone-Marrow_c-kit)", "Basophil(Bone-Marrow_c-kit)",
      "Multipotent progenitor_Ctla2a high(Bone-Marrow_c-kit)", "Eosinophil progenitor cell(Bone-Marrow_c-kit)"
    ),
    visual_check = "ok",
    dynamic_process = "differentiation"
  ),

  list(
    tissue = "embronic-mesenchyme",
    subid = "neuron-differentiation",
    milestone_network = tribble(
      ~from, ~to,
      "Neuronal Progenitors(Embryonic-Mesenchyme)", "Neuron_Igfbpl1_high(Embryonic-Mesenchyme)",
      "Neuron_Igfbpl1_high(Embryonic-Mesenchyme)", "Ganglion cell_Mapt high(Embryonic-Mesenchyme)",
      "Neuron_Igfbpl1_high(Embryonic-Mesenchyme)", "Ganglion cell_Cartpt_high(Embryonic-Mesenchyme)",
      "Neuron_Igfbpl1_high(Embryonic-Mesenchyme)", "Ganglion cell_Gal_high(Embryonic-Mesenchyme)",
      "Neuronal Progenitors(Embryonic-Mesenchyme)", "Neuron_Npy_high(Embryonic-Mesenchyme)"
    )
  ),

  list(
    tissue = "trophoblast-stem-cell",
    subid = "trophoblast-differentiation",
    milestone_network = tribble(
      ~from, ~to,
      "MEF(Trophoblast-Stem-Cell)", "TS_Mrpl12_high(Trophoblast-Stem-Cell)",
      "MEF(Trophoblast-Stem-Cell)", "TS_dif_epithelial(Trophoblast-Stem-Cell)",
      "MEF(Trophoblast-Stem-Cell)", "TS_Rps28_high(Trophoblast-Stem-Cell)"
    ),
    dynamic_process = "differentiation",
    visual_check = "ok"
  ),

  list(
    tissue = "fetal-liver",
    subid = "fetal-hematopoiesis",
    milestone_network = tribble(
      ~from, ~to,
      "Stem and progenitor cell(Fetal-liver)", "B cell(Fetal-liver)",
      "Stem and progenitor cell(Fetal-liver)", "Dendritic cell(Fetal-liver)",
      "Stem and progenitor cell(Fetal-liver)", "Erythroblast_Klf1 high(Fetal-liver)",
      "Erythroblast_Klf1 high(Fetal-liver)", "Erythroblast_Hba-a2 high(Fetal-liver)",
      "Stem and progenitor cell(Fetal-liver)", "Macrophage(Fetal-liver))",
      "Stem and progenitor cell(Fetal-liver)", "Mast cell_Mcpt8 high(Fetal-liver)",
      "Stem and progenitor cell(Fetal-liver)", "Megakaryocyte(Fetal-liver)",
      "Stem and progenitor cell(Fetal-liver)", "Neutrophil_Elane high(Fetal-liver)",
      "Neutrophil_Elane high(Fetal-liver)", "Neutrophil_Ngp high(Fetal-liver)"
    ),
    dynamic_process = "differentiation",
    visual_check = "ok"
  ),

  list(
    tissue = "mammary-gland-involution",
    subid = "endothelial-cell-aqp1-gradient",
    milestone_network = tribble(
      ~from, ~to, ~directed,
      "Endothelial cell_Aqp1 high(Mammary-Gland-Involution)", "Endothelial cell_Fabp4&Aqp1 high(Mammary-Gland-Involution)", FALSE,
      "Endothelial cell_Fabp4 high(Mammary-Gland-Involution)", "Endothelial cell_Fabp4&Aqp1 high(Mammary-Gland-Involution)", FALSE
    ),
    dynamic_process = "gradient",
    visual_check = "ok"
  ),

  list(
    tissue = "neonatal-rib",
    subid = "cartilage",
    milestone_network = tribble(
      ~from, ~to, ~directed,
      "Cartilage cell_Cxcl14 high(Neonatal-Rib)", "Cartilage cell_Clu high(Neonatal-Rib)", FALSE,
      "Cartilage cell_Cxcl14 high(Neonatal-Rib)", "Cartilage cell_Prg4 high(Neonatal-Rib)", FALSE,
      "Cartilage cell_Cxcl14 high(Neonatal-Rib)", "Cartilage cell_Col2a1 high(Neonatal-Rib)", FALSE,
      "Cartilage cell_Col2a1 high(Neonatal-Rib)", "Cartilage cell_Ppa1 high(Neonatal-Rib)", FALSE
    ),
    dynamic_process = "gradient",
    visual_check = "ok"
  ),

  list(
    tissue = "placenta",
    subid = "trophoblast-differentiation",
    milestone_network = tribble(
      ~from, ~to,
      "Progenitor trophoblast_Gjb3 high(Placenta)", "Spiral artery trophoblast giant cells(Placenta)",
      "Progenitor trophoblast_Gjb3 high(Placenta)", "Spongiotrophoblast_Phlda2 high(Placenta)",
      "Spongiotrophoblast_Phlda2 high(Placenta)", "Spongiotrophoblast_Hsd11b2 high(Placenta)",
      "Progenitor trophoblast_Gjb3 high(Placenta)", "Labyrinthine trophoblast(Placenta)"
    ),
    dynamic_process = "differentiation",
    visual_check = "ok"
  ),

  list(
    tissue = "placenta",
    subid = "trophoblast-differentiation-invasive",
    milestone_network = tribble(
      ~from, ~to,
      "Progenitor trophoblast_Gjb3 high(Placenta)", "Spiral artery trophoblast giant cells(Placenta)",
      "Progenitor trophoblast_Gjb3 high(Placenta)", "Spongiotrophoblast_Phlda2 high(Placenta)",
      "Spongiotrophoblast_Phlda2 high(Placenta)", "Spongiotrophoblast_Hsd11b2 high(Placenta)",
      "Progenitor trophoblast_Gjb3 high(Placenta)", "Labyrinthine trophoblast(Placenta)",
      "Trophoblast progenitor_Taf7l high(Placenta)", "Invasive spongiotrophoblast(Placenta)"
    ),
    dynamic_process = "differentiation",
    visual_check = "ok"
  ),

  list(
    tissue = "bone-marrow-mesenchyme",
    subid = "erythrocyte-differentiation",
    milestone_network = tribble(
      ~from, ~to,
      "Erythroblast(Bone_Marrow_Mesenchyme)", "Proerythrocytes(Bone_Marrow_Mesenchyme)",
      "Proerythrocytes(Bone_Marrow_Mesenchyme)", "Erythrocytes(Bone_Marrow_Mesenchyme)"
    ),
    dynamic_process = "differentiation",
    visual_check = "ok"
  )
)


for (setting in settings) {
  milestone_network <- setting$milestone_network
  milestone_ids <- unique(c(milestone_network$from, milestone_network$to))

  if (!"length" %in% names(milestone_network)) milestone_network$length <- 1
  if (!"directed" %in% names(milestone_network)) milestone_network$directed <- TRUE

  cell_info <- all_cell_info %>% filter(group_id %in% milestone_ids)

  grouping <- cell_info %>% select(cell_id, group_id) %>% deframe()

  counts <- get_counts(cell_info)

  id <- paste0("real/", setting$tissue, "-", setting$subid, "_", "mca")

  counts <- counts[, !(colnames(counts) %>% str_detect("mt-.*"))] # remove mitochondrial genes, as these were already used for filtering in the original study

  preprocess_dataset(
    id,
    counts,
    milestone_network,
    grouping,
    cell_info = cell_info,
    filter_hvg = FALSE
  )
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
