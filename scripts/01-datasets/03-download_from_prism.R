library(dynbenchmark)
library(tidyverse)

experiment("01-datasets")

qsub::rsync_remote(
  remote_src = TRUE,
  path_src = derived_file(remote = TRUE),
  remote_dest = FALSE,
  path_dest = derived_file(remote = FALSE),
  verbose = TRUE
)


load_datasets(list_datasets() %>% filter(dataset_source == "synthetic/prosstt") %>% pull(dataset_id)) %>% mapdf(function(dataset) {
  print(dataset$id)

  dataset$feature_info <- tibble(feature_id = colnames(dataset$counts()))
  dataset$dataset_source <- "synthetic/prosstt"

  save_dataset(dataset, dataset$id)
  TRUE
})













dataset_ids <- list_datasets() %>% filter(dataset_source == "synthetic/dyntoy") %>% pull(dataset_id)
datasets <- load_datasets()
pryr::object_size(datasets)

datasets$dataset_source

dataset <- datasets %>% filter(trajectory_type == "directed_linear") %>% extract_row_to_list(3) %>% add_root()
dataset$trajectory_type

dynplot::plot_dimred(dataset, dimred = dyndimred::dimred_pca)
dynplot::plot_heatmap(dataset)






datasets <- load_datasets()
datasets <- datasets %>%
  mutate(
    n_cells = map_int(cell_ids, length),
    n_features = map_int(feature_info, nrow)
  )

datasets %>%
  ggplot(aes(n_cells, n_features)) +
    geom_point(aes(color = dataset_source))
