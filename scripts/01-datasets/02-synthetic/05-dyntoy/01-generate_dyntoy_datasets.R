library(dynbenchmark)

toy_datasets <- dyntoy::toy_datasets
toy_datasets$dataset_source <- "synthetic/dyntoy"

datasets <- mapdf(toy_datasets, as.list)
dataset_ids <- paste0("synthetic/dyn", toy_datasets$id)

purrr::walk2(datasets, dataset_ids, save_dataset)
