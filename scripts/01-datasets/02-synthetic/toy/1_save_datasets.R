library(dynbenchmark)

toy_datasets <- dyntoy::toy_datasets

datasets <- mapdf(toy_datasets, as.list)
dataset_ids <- toy_datasets$id

walk2(datasets, dataset_ids, save_dataset)
