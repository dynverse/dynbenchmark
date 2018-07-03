library(tidyverse)
library(dynbenchmark)

dataset_preprocessing("real/mouse-cell-atlas_guo")

counts_file <- download_dataset_file("GSM2906401_BoneMarrowcKit1_dge.txt.gz", "https://www.ncbi.nlm.nih.gov/geo/download/?acc=GSM2906401&format=file&file=GSM2906401%5FBoneMarrowcKit1%5Fdge%2Etxt%2Egz")

cell_assignments_file <- download_dataset_file("MCA_CellAssignments.csv", "https://ndownloader.figshare.com/files/11083451?private_link=865e694ad06d5857db4b")


counts_file_unzipped <- counts_file %>% gsub("\\.gz", "", .)
read_lines(counts_file) %>% {.[[1]] <- paste0("\"gene\" ", .[[1]]); .} %>% write_lines(counts_file_unzipped)

counts <- read_delim(counts_file_unzipped, " ")
counts <- counts %>% as.data.frame() %>% column_to_rownames("gene") %>% as.matrix() %>% t
all_cell_info <- read_csv(cell_assignments_file) %>%
  rename(cell_id = Cell.name, cluster_id = ClusterID, tissue = Tissue, batch = Batch, barcode = Cell.Barcode, group_id = Annotation) %>%
  mutate(cell_type = gsub("([^\\(_]*).*", "\\1", group_id))

cell_info <- all_cell_info %>%
  filter(cell_id %in% rownames(counts)) %>%
  filter(
    tissue == "Bone-Marrow_c-kit"
  ) %>%
  # filter(str_detect(group_id, "Neutrophil")) %>%
  invisible()

# counts_oi <- counts[cell_info$cell_id, ]
counts_oi <- counts[cell_info %>% group_by(group_id) %>% sample_frac(0.2) %>% pull(cell_id), ]

expression_oi <- log2(counts_oi + 1)

dim(expression_oi)

dimred <- SCORPIUS::reduce_dimensionality(counts_oi, dist_fun = SCORPIUS::correlation_distance, ndim = 2, num_landmarks = 50) %>% magrittr::set_colnames(c("comp_1", "comp_2"))
dimred <- dyndimred::dimred_pca(expression_oi)
dimred <- dyndimred::dimred_umap(expression_oi)
dimred <- dyndimred::dimred_dp(expression_oi)
dimred <- dyndimred::dimred_mds(expression_oi)
dimred %>%
  as.data.frame() %>%
  rownames_to_column("cell_id") %>%
  left_join(cell_info, "cell_id") %>%
  ggplot() + geom_point(aes(comp_1, comp_2, color = cell_type)) + coord_equal()


dimred



list(
  list(
    tissue = "kidney",
    subid = "bursh-border-to-s1",
    milestone_network = tribble(
      ~from, ~to, ~directed,
      "Proximal tubule brush border cell(Kidney)", "Proximal tubule cell_Cyp4a14 high(Kidney)", FALSE,
      "Proximal tubule cell_Cyp4a14 high(Kidney)", "S1 proximal tubule cells(Kidney)", FALSE
    ),
    dynamic_process = "spatial"
  ),
  list(
    tissue = "kidney",
    subid = "distal-convoluted-tubule",
    milestone_network = tribble(
      ~from, ~to, ~directed,
      "Distal convoluted tubule_Pvalb high(Kidney)", "Distal convoluted tubule_S100g high(Kidney)", FALSE,
      "Distal convoluted tubule_S100g high(Kidney)", "Distal collecting duct principal cell_Hsd11b2 high(Kidney)", FALSE
    )
  ),
  # list(
  #   tissue = "embronic-mesenchyme",
  #   subid = "neuron-differentiation",
  #   milestone_network = tribble(
  #     ~from, ~to,
  #     "Neuronal Progenitors(Embryonic-Mesenchyme)", "Neuron_Igfbpl1_high(Embryonic-Mesenchyme)",
  #     "Neuron_Igfbpl1_high(Embryonic-Mesenchyme)", "Ganglion cell_Mapt high(Embryonic-Mesenchyme)",
  #     "Neuron_Igfbpl1_high(Embryonic-Mesenchyme)", "Ganglion cell_Cartpt_high(Embryonic-Mesenchyme)",
  #     "Neuron_Igfbpl1_high(Embryonic-Mesenchyme)", "Ganglion cell_Gal_high(Embryonic-Mesenchyme)",
  #     "Neuronal Progenitors(Embryonic-Mesenchyme)", "Neuron_Npy_high(Embryonic-Mesenchyme)",
  #   )
  # ),
  list(
    tissue = "embryonic-mesenchyme",
    subid = "stromal-cell-cxcl14-cxcl12-axis",
    milestone_network = tribble(
      ~from, ~to, ~directed,
      "Stromal Cell_Cxcl14 high(Embryonic-Mesenchyme)", "Stromal Cell_Agtr2 high(Embryonic-Mesenchyme)", FALSE,
      "Stromal Cell_Agtr2 high(Embryonic-Mesenchyme)", "Stromal cell_Cxcl12 high(Embryonic-Mesenchyme)", FALSE
    ),
    sources = c("10.1038/s41598-017-17490-z")
  ),
  list(
    tissue = "bone-marrow-c-kit",
    subid = "hematopoiesis",
    milestone_network = tribble(
      ~from, ~to,
      "Multipotent progenitor_Ctla2a high(Bone-Marrow_c-kit)", "Monocyte progenitor(Bone-Marrow_c-kit)",
    )
  ),

  list(
    tissue = "bone-marrow-c-kit",
    subid = "monocyte-differentiation",
    milestone_network = tribble(
      ~from, ~to,
      "Monocyte progenitor cell_Prtn3 high(Bone-Marrow_c-kit)", "Monocyte progenitor cell_Ctsg high(Bone-Marrow_c-kit)",
      "Monocyte progenitor cell_Ctsg high(Bone-Marrow_c-kit)", "Monocyte progenitor(Bone-Marrow_c-kit)"
    ),
    visual_check = "ok"
  )
)
