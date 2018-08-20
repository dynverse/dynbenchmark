library(tidyverse)
library(dynbenchmark)
library(patchwork)
library(tidytext)
library(svglite)

experiment("03-method_characterisation")

methods <- read_rds(derived_file("methods.rds")) %>%
  filter(type == "algorithm") # only really interested in algorithms

method_components <- methods %>% select(id, components) %>%
  mutate(components = gsub("\\(.*?\\)", "", components)) %>%  # remove anything between ()
  unnest_tokens(component_id, components, "regex", pattern = "[\\{\\}\\[\\]\\|\\+\\ = ]", to_lower = FALSE) %>%
  mutate(component_id = trimws(component_id)) %>%
  filter(nchar(component_id) > 0)

component_categories <- tribble(
  ~component_id, ~category,
  "principal curves", "ordering",
  "PCA", "dimensionality reduction",
  "kNN", "graph building",
  "MST", "graph building",
  "k-medoids", "clustering",
  "ICA", "dimensionality reduction",
  "GPLVM", "generative model",
  "GPLVM", "dimensionality reduction",
  "GPLVM", "ordering",
  "any multiple dimensionality reductions", "dimensionality reduction",
  "any dimensionality reduction", "dimensionality reduction",
  "diffusion map", "dimensionality reduction",
  "k-means", "clustering",
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
  "laplacian eigenmap", "dimensionality reduction",
  "EM clustering", "clustering",
  "consensus clustering", "clustering",
  "weighted cluster graphs", "graph building",
  "maximum likelihood ordering", "ordering",
  "LDA", "generative model",
  "Bayesian hierarchical mixture of factor analyzers", "generative model",
  "TSP", "graph building",
  "TSP", "graph building",
  "hierarchical clustering", "clustering",
  "SST", "graph building",
  "IMC", "graph building",
  "longest shortest path", "ordering",
  "cell projection to tree backbone", "ordering",
  "find landmarks/waypoints", "graph building",
  "mixture OU process", "generative model",
  "Morphing Gaussian Mixture", "generative model",
  "shortest path to start", "ordering",
  "latent variable model", "generative model",
  "PQ tree", "graph building",
) %>% group_by(component_id) %>%
  filter(row_number() == 1)

categories <- component_categories$category %>% unique() %>% discard(is.na)

component_category_colors <- RColorBrewer::brewer.pal(length(categories), "Set2") %>% set_names(categories)

write_rds(method_components, derived_file("method_components.rds"))
write_rds(component_categories, derived_file("component_categories.rds"))
write_rds(component_category_colors, derived_file("component_category_colors.rds"))

method_component_counts <- method_components %>%
  count(component_id) %>%
  left_join(component_categories, "component_id")

method_components_wordcloud_plot <- method_component_counts %>%
  filter(n > 1 | !is.na(category)) %>%
  ggplot() +
    ggrepel::geom_text_repel(aes(1, 1, size = n, label = component_id, color = category), segment.size = 0) +
    scale_y_continuous(breaks = NULL) +
    scale_x_continuous(breaks = NULL) +
    theme_void() +
    labs(x = '', y = '') +
    scale_size(range = c(3, 15), guide = FALSE) +
    scale_color_manual(guide = F, values = component_category_colors)
method_components_wordcloud_plot

method_components_ordering_plot <- method_component_counts %>%
  arrange(n) %>%
  filter(n > 1 | !is.na(category)) %>%
  mutate(component_id = forcats::fct_inorder(component_id)) %>%
  ggplot(aes(component_id, n, fill = category)) +
    geom_bar(stat = "identity") +
    geom_text(aes(y = 0, label = pritt("{component_id}: {n}")), stat = "identity", hjust = 0) +
    scale_x_discrete(breaks = c(), expand = c(0, 0), name = "") +
    scale_y_continuous(label_long("n_methods"), expand = c(0, 0), breaks = seq_len(500)) +
    scale_fill_manual(values = component_category_colors) +
    coord_flip() +
    theme(legend.position = "top")
method_components_ordering_plot

n_methods <- length(unique(method_components$method_id))
category_counts <- method_components %>%
  left_join(component_categories, "component_id") %>%
  group_by(method_id, category) %>%
  summarise() %>%
  group_by(category) %>%
  count() %>%
  filter(!is.na(category))

method_components_categories_plot <- bind_rows(
  category_counts %>% mutate(included = category),
  category_counts %>% mutate(included = "unknown", n = n_methods - n)
) %>%
  mutate(included = fct_inorder(included)) %>%
  ggplot(aes(1, n)) +
    geom_bar(aes(fill = included), stat = "identity") +
    geom_text(aes(label = n, y = n_methods - 4), fill = NA, category_counts, size = 5) +
    facet_grid(.~category, labeller = label_facet()) +
    scale_fill_manual(values = component_category_colors %>% c(unknown = "grey")) +
    coord_polar("y") +
    theme_void() +
    theme(legend.position = "none")
method_components_categories_plot




##  ............................................................................
##  method components plot                                                  ####
method_components_plot <-
  cowplot::plot_grid(method_components_wordcloud_plot, method_components_ordering_plot, rel_widths = c(0.6, 0.4), labels = "auto") %>%
  cowplot::plot_grid(., method_components_categories_plot, ncol = 1, rel_heights = c(0.6, 0.4), labels = c("", "c"))
method_components_plot

cowplot::save_plot(result_file("method_components.svg"), method_components_plot, base_width=15, base_height=12)

