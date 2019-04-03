#' List and load the platforms
#' @export
load_platforms <- function() {
  folder <- result_file("", experiment_id = "01-datasets/02-synthetic/platforms")
  list.files(folder, recursive = TRUE, full.names = TRUE) %>% gsub("(.*)\\.tsv", "\\1", .) %>% map(read_rds)
}

#' Load a platform
#' @rdname load_platforms
#' @param platform_id Platform identifier
#' @export
load_platform <- function(platform_id) {
  folder <- result_file("", experiment_id = "01-datasets/02-synthetic/platforms")
  read_rds(paste0(folder, "/", platform_id, ".rds"))
}

#' @rdname load_platforms
#' @param platforms The platforms, loaded by load_platforms
#' @export
get_platform_features <- function(platforms) {
  features <- map(platforms, function(platform) {
    c(
      platform[c("trajectory_dependent_features", "n_cells", "n_features")],
      platform$estimate %>% attributes() %>% keep(is.numeric)
    )
  }) %>% bind_rows() %>% as.matrix() %>% scale
  rownames(features) <- map(platforms, "platform_id")
  features <- features[, apply(features, 2, function(x) !any(is.na(x)))]

  features
}

#' @rdname load_platforms
#' @param n_platforms Number of platforms to select
#' @export
select_platforms <- function(n_platforms) {
  platforms <- load_platforms()

  if (n_platforms == length(platforms)) {
    return(platforms)
  } else if (n_platforms > length(platforms)) {
    stop("Not enough platforms available (", length(platforms), ") for requested number of platforms (", n_platforms, ")")
  }

  # select features
  features <- get_platform_features(platforms)

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


#' Estimate a platform
#' @param dataset_id The dataset_id from which the platform will be estimated, using the files in `datasets_preproc/raw``
#' @inheritParams dyngen::get_platform_from_counts
#' @importFrom dyngen get_platform_from_counts
#' @export
estimate_platform <- function(dataset_id, subsample = 500) {
  platform_location <- derived_file(paste0(dataset_id, ".rds"), experiment_id = "01-platforms")
  if (!file.exists(dirname(platform_location))) dir.create(dirname(platform_location), recursive = TRUE)

  platform <- load_or_generate(
    platform_location,
    {
      dataset_raw <- read_rds(dataset_raw_file(dataset_id))

      counts <- dataset_raw$counts
      grouping <- dataset_raw$grouping

      dyngen::get_platform_from_counts(counts, grouping, subsample = subsample)
    }
  )

  platform$platform_id <- dataset_id
  platform
}

