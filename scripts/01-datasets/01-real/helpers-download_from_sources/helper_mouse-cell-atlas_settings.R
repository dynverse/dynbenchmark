settings <- list(
  list(
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
    tissue = "embronic-mesenchyme",
    subid = "neuron-differentiation",
    milestone_network = tribble(
      ~from, ~to,
      "Neuronal Progenitors(Embryonic-Mesenchyme)", "Neuron_Igfbpl1 high(Embryonic-Mesenchyme)",
      "Neuron_Igfbpl1 high(Embryonic-Mesenchyme)", "Ganglion cell_Mapt high(Embryonic-Mesenchyme)",
      "Neuron_Igfbpl1 high(Embryonic-Mesenchyme)", "Ganglion cell_Cartpt high(Embryonic-Mesenchyme)",
      "Neuron_Igfbpl1 high(Embryonic-Mesenchyme)", "Ganglion cell_Gal high(Embryonic-Mesenchyme)",
      "Neuronal Progenitors(Embryonic-Mesenchyme)", "Neuron_Npy high(Embryonic-Mesenchyme)"
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


# create ids
settings <- map(settings, function(setting) {
  setting$id <- paste0("real/silver/", setting$tissue, "-", setting$subid, "_", "mca")
  setting
})

# combine differrent datasets to create disconnected trajectories
set.seed(1)
combinations <- combn(seq_along(settings), 2, simplify=F) %>% sample(10)
settings <- c(
  settings,
  map2(seq_along(combinations), combinations, function(combination_ix, combination) {
    setting <- list(
      milestone_network = bind_rows(map(settings[combination], "milestone_network")),
      dynamic_process = "mix",
      id = paste0("real/silver/mouse-cell-atlas-combination-", combination_ix)
    )
  })
)
