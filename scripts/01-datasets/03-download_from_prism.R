library(dynbenchmark)
library(tidyverse)

experiment("01-datasets")

qsub::rsync_remote(
  remote_src = TRUE,
  path_src = derived_file(remote = TRUE),
  remote_dest = FALSE,
  path_dest = derived_file(remote = FALSE),
  verbose = TRUE,
  compress = FALSE
)

# # Upload to prism
# qsub::rsync_remote(
#   remote_src = FALSE,
#   path_src = derived_file(remote = FALSE),
#   remote_dest = TRUE,
#   path_dest = derived_file(remote = TRUE),
#   verbose = TRUE
# )





datasets <- load_datasets()
# datasets <- load_datasets()
datasets <- datasets %>%
  mutate(
    n_cells = map_int(cell_ids, length),
    n_features = map_int(feature_info, nrow)
  )

datasets %>%
  ggplot(aes(n_cells, n_features)) +
  geom_point(aes(color = source)) +
  scale_x_log10() +
  scale_y_log10()




dataset <- load_dataset(list_datasets() %>% filter(source == "synthetic/dyngen") %>% sample_n(1) %>% pull(id))
plot_dimred(dataset, dimred = dyndimred::dimred_landmark_mds)


datasets <- list_datasets() %>% group_by(source) %>% sample_n(2) %>% pull(id) %>% load_datasets()

datasets$plot_dimred <- mapdf(datasets, ~plot_dimred(., dimred = dyndimred::dimred_landmark_mds) + ggtitle(.$id))
patchwork::wrap_plots(datasets$plot_dimred)

plots <- datasets %>%
  group_by(source) %>%
  sample_n(1) %>%
  ungroup() %>%
  mapdf(~plot_dimred(.))

plots[[5]]
