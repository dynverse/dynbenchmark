#' Estimation of the platforms from real data done by `dynbenchmark::estimate_platform`

library(tidyverse)
library(dynbenchmark)
library(qsub)

experiment("01-datasets/02-synthetic")

# remove all platforms
rm_remote(result_file("platforms", remote = TRUE), remote = TRUE, recursive = TRUE)

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
  override_fun = TRUE
)

write_rds(handle, derived_file("handle.rds"))
handle <- read_rds(derived_file("handle.rds"))

qsub_retrieve(handle)

# sync back locally
qsub::rsync_remote(
  remote_src = TRUE,
  path_src = result_file("platforms", remote = TRUE),
  remote_dest = FALSE,
  path_dest = derived_file("platforms", remote = FALSE),
  verbose = TRUE
)

# correlations between platforms
platforms <- load_platforms()
metadata <- read_rds(result_file("metadata.rds", "01-datasets/01-real"))

features <- get_platform_features(platforms)

features_dimred <- dyndimred::dimred_mds(features) %>%
  as.data.frame() %>%
  rownames_to_column("id") %>%
  left_join(metadata)

platforms_selected <- select_platforms(10) %>% map("platform_id")
features_dimred$selected <- features_dimred$id %in% platforms_selected

base_plot <- ggplot(features_dimred, aes(comp_1, comp_2)) +
  geom_point(aes(alpha = selected), size = 5, shape = 21, color = "black") +
  scale_alpha_manual(values = c(`TRUE` = 1, `FALSE` = 0)) +
  theme_graph()

plots <- lst(
  base_plot + geom_point(aes(color = technology)) + ggtitle("Technologies"),
  base_plot + geom_point(aes(color = n_cells)) + scale_color_viridis_c(trans = "log", breaks = c(100, 1000, 10000)) + ggtitle("Dimensions"),
  base_plot + geom_point(aes(color = standard)) + ggtitle("Standard")
)

plot_platform_diversity <- patchwork::wrap_plots(plots, ncol = 2)
write_rds(plot_platform_diversity, result_file("platform_diversity.rds"))



