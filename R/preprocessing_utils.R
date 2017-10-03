save_dataset <- function(dataset) {
  write_rds(dataset, paste0("analysis/data/datasets/real/", dataset$info$id, ".rds"))
}
