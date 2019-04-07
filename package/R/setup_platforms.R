#' Estimate platform parameters from a dataset
#'
#' Altenatively, [platform_simple()] returns a toy platform parameter configuration.
#'
#' @param counts The counts with cells in rows and genes in columns.
#' @param grouping A named vector representing a grouping of the cells.
#' @param subsample The number of cells to subsample.
#'
#' @rdname platform
#'
#' @export
platform_from_counts <- function(counts, grouping, subsample = 500) {
  requireNamespace("splatter")

  # add a try catch to the splatEstDropout function because it errors too frequently
  old_fun <- splatter:::splatEstDropout
  new_fun <- function(...) {
    tryCatch({
      old_fun(...)
    }, error = function(e) {
      warning("Could not estimate dropout parameters, defaulting to mid = 0.01 and shape = 1.")
      splatter::setParams(params, dropout.mid = 0.01, dropout.shape = 1)
    })

  }
  assignInNamespace("splatEstDropout", new_fun, asNamespace("splatter"))
  on.exit(assignInNamespace("splatEstDropout", old_fun, asNamespace("splatter")))

  # remove genes that are not sufficiently expressed
  min_pct <- 0.05
  counts <- counts[, apply(counts, 2, function(x) mean(x > 0) > min_pct), drop = FALSE]

  # sample the number of
  if (!is.null(subsample)) {
    ix <- sample.int(nrow(counts), min(nrow(counts), subsample))
  } else {
    ix <- seq_len(nrow(counts))
  }

  # estimate splatter params
  estimate <- splatter::splatEstimate(t(counts[ix, , drop = FALSE]))
  attr(class(estimate), "package") <- NULL # make sure scater won't get loaded when the platform is loaded

  # determine how many features change between trajectory stages
  group_ids <- unique(dataset_raw$grouping)

  # differential expression using wilcox test
  diffexp <- map_df(group_ids, function(group_id) {
    inside <- dataset_raw$grouping == group_id
    outside <- dataset_raw$grouping != group_id

    expression_inside <- log2(counts[inside, ] + 1)
    expression_outside <- log2(counts[outside, ] + 1)

    result <- map_df(colnames(expression_inside), function(feature_id) {
      pvalue <- wilcox.test(expression_inside[, feature_id], expression_outside[, feature_id])$p.value
      log2fc <- mean(expression_inside[, feature_id]) - mean(expression_outside[, feature_id])

      tibble(
        pvalue = pvalue,
        log2fc = log2fc,
        feature_id = feature_id,
        group_id = group_id
      )
    })
  }) %>%
    mutate(qvalue = p.adjust(pvalue, "fdr"))

  qvalue_cutoff <- 0.05
  log2fc_cutoff <- 1
  diffexp_features <- diffexp %>% filter(
    qvalue < qvalue_cutoff,
    abs(log2fc) > log2fc_cutoff
  ) %>%
    pull(feature_id) %>%
    unique()

  pct_main_features <- length(diffexp_features) / ncol(dataset_raw$counts)

  # create platform object
  lst(
    estimate,
    pct_main_features,
    num_cells = nrow(counts),
    num_features = ncol(counts)
  )
}

#' @param n_cells The number of cells
#' @param n_features The number of features
#' @param pct_main_features The percentage of features that are being driven by the trajectory (or vice versa)
#' @param dropout_rate The mean rate of dropouts
#' @param dropout_shape The shape of dropouts
#'
#' @rdname platform
#'
#' @export
platform_simple <- function(
  n_cells = 100L,
  n_features = 1000L,
  pct_main_features = 0.5,
  dropout_rate = 0.01,
  dropout_shape = 1
) {
  list(
    platform_id = "simple",
    estimate = splatter::newSplatParams(mean.rate = dropout_rate, mean.shape = dropout_shape),
    num_cells = n_cells,
    num_features = n_features,
    pct_main_features = pct_main_features
  )
}






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
#' @inheritParams platform_from_counts
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

      platform_from_counts(counts, grouping, subsample = subsample)
    }
  )

  platform$platform_id <- dataset_id
  platform
}

