#' List and load the platforms
#' @export
load_platforms <- function() {
  folder <- derived_file("", experiment_id = "01-datasets_preproc/platforms/real")
  list.files(folder, recursive = TRUE, full.names = TRUE) %>% gsub("(.*)\\.tsv", "\\1", .) %>% map(read_rds)
}


#' @rdname load_platforms
#' @param n_platforms Number of platforms to select
#' @export
select_platforms <- function(n_platforms) {
  platforms <- load_platforms()

  # select features
  features <- map(platforms, function(platform) {
    c(
      platform[c("trajectory_dependent_features", "n_cells", "n_features")],
      platform$estimate %>% attributes() %>% keep(is.numeric)
    )
  }) %>% bind_rows() %>% as.matrix() %>% scale
  rownames(features) <- map(platforms, "platform_id")
  features <- features[, apply(features, 2, function(x) !any(is.na(x)))]

  # cluster the platforms according to these features
  corm <- cor(t(features))
  clust <- cluster::pam(corm, n_platforms)

  # select from each cluster a random platform
  platform_ids <- clust$clustering %>%
    enframe("platform_id", "cluster") %>%
    group_by(cluster) %>%
    sample_n(1) %>%
    pull(platform_id)

  # return platforms
  platforms %>% keep(~.$platform_id %in% platform_ids)
}
