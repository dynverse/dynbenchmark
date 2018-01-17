library(tidyverse)
library(dynalysis)
library(cowplot)

experiment("method_characteristics")

source("analysis/analyses/4-method_characterization/0_common.R")

method_df_evaluated <- read_rds(derived_file("method_df_evaluated.rds"))


library(tidytext)
library(wordcloud)
method_components <- method_df_evaluated %>% select(name, components) %>%
  mutate(components = gsub("\\(.*\\)", "", components)) %>%  # remove anything between ()
  unnest_tokens(component, components, "regex", pattern="[\\{\\}\\[\\]\\|\\+\\=]", to_lower=FALSE) %>%
  mutate(component = trimws(component)) %>%
  filter(nchar(component) > 0)

categories <- tribble(
  ~component, ~category,
  "principal curves", "ordering",
  "PCA", "dimensionality reduction",
  "knn graph", "graph building",
  "MST", "graph building",
  "k-medoids", "clustering",
  "ICA", "dimensionality reduction",
  "GPLVM", "dimensionality reduction",
  "GPLVM", "ordering",
  "any multiple dimensionality reductions", "dimensionality reduction",
  "any dimensionality reduction", "dimensionality reduction",
  "diffusion map", "dimensionality reduction",
  "k-medoids clustering", "clustering",
  "kmeans", "clustering",
  "LLE", "dimensionality reduction",
  "MDS", "dimensionality reduction",
  "DDRTree", "graph building",
  "DDRTree", "dimensionality reduction",
  "mclust", "clustering",
  "tSNE", "dimensionality reduction",
  "sammon", "dimensionality reduction",
  "louvain clustering", "clustering",
  "weighted neighbourhood network by number of cells", "graph building",
  "dynamic clustering", "clustering",
  "laplacian eigenmap", "dimensionality reduction"
) %>% group_by(component) %>%
  filter(row_number() == 1)


method_component_counts <- method_components %>%
  count(component) %>%
  left_join(categories, "component")


method_components_wordcloud_plot <- method_component_counts %>%
  filter(n > 1 | !is.na(category)) %>%
  ggplot() +
    ggrepel::geom_text_repel(aes(1, 1, size=n, label=component, color=category), segment.size = 0) +
    scale_y_continuous(breaks = NULL) +
    scale_x_continuous(breaks = NULL) +
    theme_void() +
    labs(x = '', y = '') +
    scale_size(range = c(3, 15), guide = FALSE) +
    scale_color_discrete(guide=F)
method_components_wordcloud_plot

method_components_ordering_plot <- method_component_counts %>%
  arrange(n) %>%
  filter(n > 1 | !is.na(category)) %>%
  mutate(component = factor(component, levels=unique(component))) %>%
  ggplot(aes(component, n, fill = category)) +
    geom_bar(stat="identity") +
    geom_text(aes(y=0, label=component), stat="identity", hjust=0) +
    scale_x_discrete(breaks=c(), expand=c(0, 0), name="") +
    scale_y_continuous(expand=c(0, 0), breaks=seq(0, 10), name="# methods") +
    coord_flip() +
    theme(legend.position="top")
method_components_ordering_plot

method_components_plot <- cowplot::plot_grid(method_components_wordcloud_plot, method_components_ordering_plot)
method_components_plot


saveRDS(method_components_plot, figure_file("method_components_plot.rds"))
