#' @export
dataset_location <- function(dataset_id, prefix="") {
  if(file.exists("/group/irc/shared")) {
    paste0("/group/irc/shared/dynalysis/analysis/data/datasets/", prefix, "/", dataset_id, ".rds")
  } else {
    paste0("analysis/data/datasets/", prefix, "/", dataset_id, ".rds")
  }
}


#' @export
load_dataset <- function(dataset_id, prefix="") {
  read_rds(dataset_location(dataset_id, prefix))
}
