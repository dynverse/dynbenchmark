#' @export
load_dataset <- function(dataset_id, prefix="") {
  if(file.exists("/group/irc/shared")) {
    read_rds(paste0("/group/irc/shared/dynalysis/analysis/data/datasets/", prefix, "/", dataset_id, ".rds"))
  } else {
    read_rds(paste0("analysis/data/datasets/", prefix, "/", dataset_id, ".rds"))
  }
}

#' @export
save_dataset <- function(dataset) {
  write_rds(dataset, paste0("analysis/data/datasets/real/", dataset$info$id, ".rds"))
}
