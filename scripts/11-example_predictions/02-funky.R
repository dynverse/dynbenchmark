library(dynbenchmark)
library(tidyverse)

experiment("11-example_predictions")

source(scripts_file("helper-funky.R"))

folder <- "../../dyndocs/funky_cover/data/embeddings"
dir.create(folder, showWarnings = FALSE)

dataset_ids <- c(
  "synthetic/dyntoy/bifurcating_3",
  "real/gold/developing-dendritic-cells_schlitzer",
  "real/silver/fibroblast-reprogramming_treutlein",
  "synthetic/dyntoy/disconnected_1",
  "synthetic/dyngen/72",
  "synthetic/dyntoy/linear_1",
  "synthetic/dyntoy/cyclic_7",
  "synthetic/dyntoy/diverging_with_loops_5",
  "synthetic/dyntoy/tree_5"
)

walk(seq_along(dataset_ids), function(dataset_ix) {
  dataset_id <- dataset_ids[dataset_ix]

  dataset <- load_dataset(dataset_id)
  plot_dimred(dataset)

  dimred <- dyndimred::dimred_landmark_mds(dataset$expression(), ndim = 3)

  cell_positions <- dimred %>%
    as.data.frame() %>%
    rownames_to_column("cell_id") %>%
    mutate_at(vars(comp_1, comp_2, comp_3), dynutils::scale_minmax)
  projection <- project_waypoints_multidim(dataset, cell_positions)


  cell_positions %>%
    sample_n(50) %>%
    write_csv(fs::path(folder, paste0("embedding_", dataset_ix, ".csv")))

  projection$edges$group = projection$edges %>% group_by(from_milestone_id, to_milestone_id) %>% group_indices()
  projection$edges %>%
    select(comp_1 = comp_1_from, comp_2 = comp_2_from, comp_3 = comp_3_from, group) %>%
    write_csv(fs::path(folder, paste0("trajectory_", dataset_ix, ".csv")))
})
