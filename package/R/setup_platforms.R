#' List and load the platforms
#' @export
load_platforms <- function() {
  folder <- derived_file("", experiment_id = "01-datasets_preproc/platforms/real")
  list.files(folder, recursive = TRUE, full.names = TRUE) %>% gsub("(.*)\\.tsv", "\\1", .) %>% map(read_rds)
}

#' Load a platform
#' @rdname load_platforms
#' @param platform_id Platform identifier
#' @export
load_platform <- function(platform_id) {
  folder <- derived_file("", experiment_id = "01-datasets_preproc/platforms/")
  read_rds(paste0(folder, "/", platform_id, ".rds"))
}

#' @rdname load_platforms
#' @export
load_simple_platform <- function() {
  list(
    platform_id = "simple",
    estimate = splatter::newSplatParams(mean.rate = 0.01, mean.shape = 1),
    n_cells = 100L,
    n_features = 100L,
    trajectory_dependent_features = 0.1
  )
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


# fix for splatEstDroupout function to avoid convergence errors in dropout estimation
splatEstDropout <- function(norm.counts, params) {
  means <- rowMeans(norm.counts)
  x <- log(means)
  obs.zeros <- rowSums(norm.counts == 0)
  y <- obs.zeros / ncol(norm.counts)
  df <- data.frame(x, y)
  fit <- nls(y ~ splatter:::logistic(x, x0 = x0, k = k), data = df,
             start = list(x0 = 4, k = -1)) # this initial condition has been changed, to avoid convergence errors
  mid <- summary(fit)$coefficients["x0", "Estimate"]
  shape <- summary(fit)$coefficients["k", "Estimate"]
  params <- splatter::setParams(params, dropout.mid = mid, dropout.shape = shape)
  return(params)
}

#' Estimate a platform
#' @param dataset_id The dataset_id from which the platform will be estimated, using the files in datasets_preproc/raw
#' @export
estimate_platform <- function(dataset_id) {
  requireNamespace("Seurat")
  requireNamespace("splatter")
  assignInNamespace("splatEstDropout", dynbenchmark:::splatEstDropout, asNamespace("splatter"))

  platform_location <- derived_file(paste0(dataset_id, ".rds"), experiment_id = "01-datasets_preproc/platforms")
  platform <- load_or_generate(
    platform_location,
    {
      dataset_raw <- read_rds(dataset_raw_file(dataset_id))

      counts <- dataset_raw$counts

      # determine how many features change between trajectory stages
      seurat <- Seurat::CreateSeuratObject(t(dataset_raw$counts))
      seurat@ident <- factor(dataset_raw$grouping)
      changing <- Seurat::FindAllMarkers(seurat, logfc.treshold = 1, min.pct = 0.4, test.use = "t")
      n_changing <- changing %>% pull(gene) %>% unique() %>% length()
      trajectory_dependent_features <- n_changing / ncol(counts)

      # estimate splatter params
      estimate <- splatter::splatEstimate(t(counts[sample(nrow(counts), min(nrow(counts), 500)), ]))
      class(estimate) <- "TheMuscularDogBlinkedQuietly." # change the class, so scater won't get magically loaded when the platform is loaded

      # create platform object
      platform <- lst(
        estimate,
        trajectory_dependent_features,
        n_cells = nrow(counts),
        n_features = ncol(counts),
        platform_id = dataset_id
      )
    }
  )
}
