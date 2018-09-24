#' Estimation of the platforms from real data done by `dynbenchmark::estimate_platform`

library(tidyverse)
library(dynbenchmark)
library(qsub)

experiment("01-platforms")

# remove all platforms
rm_remote(derived_file("", remote = TRUE), remote = TRUE, recursive = TRUE)

# use all real dataset for platform estimation
dataset_ids <- list_datasets("real") %>% pull(id)

# run on cluster
qsub_config <- override_qsub_config(
  name = "platform_estim",
  memory = "10G",
  wait = FALSE,
  remove_tmp_folder = FALSE,
  stop_on_error = FALSE
)

handle <- qsub_lapply(
  dataset_ids,
  qsub_config = qsub_config,
  estimate_platform,
  override_fun = T
)

write_rds(handle, derived_file("handle.rds"))
handle <- read_rds(derived_file("handle.rds"))

qsub_retrieve(handle)

# sync back locally
qsub::rsync_remote(
  remote_src = TRUE,
  path_src = derived_file(remote = TRUE),
  remote_dest = FALSE,
  path_dest = derived_file(remote = FALSE),
  verbose = TRUE
)

# correlations between platforms
platforms <- load_platforms()
metadata <- read_rds(result_file("metadata.rds", "01-datasets/01-real"))

features <- get_platform_features(platforms)

features_dimred <- dyndimred::dimred_mds(features) %>%
  as.data.frame() %>%
  rownames_to_column("platform_id") %>%






ggplot(features_dimred) + geom_point(aes(comp_1, comp_2))



# # debug
# dataset_id <- "real/silver/planaria-pharynx-differentiation_plass"
# out <- estimate_platform(dataset_id)
#
#
#
# requireNamespace("splatter")
# assignInNamespace("splatEstDropout", dynbenchmark:::splatEstDropout, asNamespace("splatter"))
#
# platform_location <- derived_file(paste0(dataset_id, ".rds"), experiment_id = "01-platforms")
# if (!file.exists(dirname(platform_location))) dir.create(dirname(platform_location), recursive = TRUE)
#
# platform <- load_or_generate(
#   platform_location,
#   {
#     dataset_raw <- read_rds(dataset_raw_file(dataset_id))
#
#     counts <- dataset_raw$counts
#
#     # determine how many features change between trajectory stages
#     group_ids <- unique(dataset_raw$grouping)
#     min.pct <- 0.4
#     counts <- dataset_raw$counts[, apply(dataset_raw$counts, 2, function(x) mean(x>0) > min.pct)]
#     diffexp <- map_df(group_ids, function(group_id) {
#       inside <- dataset_raw$grouping == group_id
#       outside <- dataset_raw$grouping != group_id
#
#       expression_inside <- log2(counts[inside, ] + 1)
#       expression_outside <- log2(counts[outside, ] + 1)
#
#       result <- map_df(colnames(expression_inside), function(feature_id) {
#         pvalue <- wilcox.test(expression_inside[, feature_id], expression_outside[, feature_id])$p.value
#         log2fc <- mean(expression_inside[, feature_id]) - mean(expression_outside[, feature_id])
#
#         tibble(
#           pvalue = pvalue,
#           log2fc = log2fc,
#           feature_id = feature_id,
#           group_id = group_id
#         )
#       })
#     })
#
#     diffexp_features <- diffexp %>% filter(
#       pvalue < 0.05,
#       abs(log2fc) > 1
#     ) %>%
#       pull(feature_id) %>%
#       unique()
#
#     trajectory_dependent_features <- length(diffexp_features) / ncol(dataset_raw$counts)
#
#     # estimate splatter params
#     estimate <- splatter::splatEstimate(t(counts[sample(nrow(counts), min(nrow(counts), 500)), ]))
#     class(estimate) <- "TheMuscularDogBlinkedQuietly." # change the class, so scater won't get magically loaded when the platform is loaded
#
#     # create platform object
#     platform <- lst(
#       estimate,
#       trajectory_dependent_features,
#       n_cells = nrow(counts),
#       n_features = ncol(counts),
#       platform_id = dataset_id
#     )
#   }
# )
