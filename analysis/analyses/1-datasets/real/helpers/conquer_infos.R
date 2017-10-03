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
    ),
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
    ),
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
    ),
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
    )
  ),
  list(
    id = "germline_human_guo",
    rds_name = "GSE63818-GPL16791",
    milestone_source = function(cell_info) gsub("(.*)_.*", "\\1", as.character(cell_info$title)),
    milestone_network = tribble(
      ~from, ~to,
      "G1", "S",
      "S", "G2",
      "G2", "G1"
    )
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
    )
  ),
  # list(
  #   id = "intestinal_organoids_grun",
  #   rds_name = "GSE62270-GPL17021",
  #   milestone_source = function(cell_info) gsub("(.*)_.*", "\\1", as.character(cell_info$title)),
  #   milestone_network = tribble(
  #     ~from, ~to,
  #     "G1", "S",
  #     "S", "G2",
  #     "G2", "G1"
  #   )
  # ),
  ######################### Do not run separately, SEE TRAPNELL2014.R
  list(
    id = "myoblast_differentiation_trapnell",
    paper = "https://www.nature.com/nbt/journal/v32/n4/full/nbt.2859.html",
    milestone_source = function(cell_info) gsub("Cell (T\\d*)_.*", "\\1", as.character(cell_info$title)),
    milestone_network = tribble(
      ~from, ~to,
      "T0", "T24",
      "T24", "T48",
      "T48", "T72"
    ),
    organism = "mus_musculus",
    source_id = "conquer"
  )
) %>% {set_names(., map(., "id"))}

