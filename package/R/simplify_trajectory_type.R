#' Convert trajectory type
#' @param trajectory_types_oi trajectory types to simplify
#' @export
simplify_trajectory_type <- function(trajectory_types_oi) {
  set_names(trajectory_types$simplified, trajectory_types$id)[trajectory_types_oi]
}
