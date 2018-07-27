library(dynbenchmark)

toy_datasets <- dyntoy::toy_datasets
toy_datasets$dataset_source <- "synthetic/dyntoy"
toy_datasets$simulation_design <- map(toy_datasets$model, function(model) {
  list(
    simulator = "dyntoy",
    simulator_version = devtools::session_info()$packages %>% filter(package %in% c("dyntoy", "dynbenchmark")),
    model = model
  )
})

datasets <- mapdf(toy_datasets, as.list)
dataset_ids <- paste0("synthetic/dyn", toy_datasets$id)

purrr::walk2(datasets, dataset_ids, save_dataset)
