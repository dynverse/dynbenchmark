library(tidyverse)
library(dynalysis)
library(cowplot)
library(tidytext)

experiment("4-method_characterisation")

implementations <- read_rds(derived_file("implementations.rds")) %>%
  filter(type == "algorithm") # only really interested in algorithms

implementation_components <- implementations %>% select(implementation_id, components) %>%
  mutate(components = gsub("\\(.*?\\)", "", components)) %>%  # remove anything between ()
  unnest_tokens(component_id, components, "regex", pattern = "[\\{\\}\\[\\]\\|\\+\\=]", to_lower = FALSE) %>%
  mutate(component_id = trimws(component_id)) %>%
  filter(nchar(component_id) > 0)

component_categories <- tribble(
  ~component_id, ~category,
  "principal curves", "ordering",
  "PCA", "dimensionality reduction",
  "knn graph", "graph building",
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
  "PQ tree", "graph building"
) %>% group_by(component_id) %>%
  filter(row_number() == 1)

categories <- component_categories$category %>% unique() %>% discard(is.na)

component_category_colors <- RColorBrewer::brewer.pal(length(categories), "Set2") %>% set_names(categories)

write_rds(implementation_components, derived_file("implementation_components.rds"))
write_rds(component_categories, derived_file("component_categories.rds"))
write_rds(component_category_colors, derived_file("component_category_colors.rds"))

implementation_component_counts <- implementation_components %>%
  count(component_id) %>%
  left_join(component_categories, "component_id")

implementation_components_wordcloud_plot <- implementation_component_counts %>%
  filter(n > 1 | !is.na(category)) %>%
  ggplot() +
    ggrepel::geom_text_repel(aes(1, 1, size=n, label=component_id, color=category), segment.size = 0) +
    scale_y_continuous(breaks = NULL) +
    scale_x_continuous(breaks = NULL) +
    theme_void() +
    labs(x = '', y = '') +
    scale_size(range = c(3, 15), guide = FALSE) +
    scale_color_manual(guide=F, values=component_category_colors)
implementation_components_wordcloud_plot

implementation_components_ordering_plot <- implementation_component_counts %>%
  arrange(n) %>%
  filter(n > 1 | !is.na(category)) %>%
  mutate(component_id = forcats::fct_inorder(component_id)) %>%
  ggplot(aes(component_id, n, fill = category)) +
    geom_bar(stat="identity") +
    geom_text(aes(y=0, label=pritt("{component_id}: {n}")), stat="identity", hjust=0) +
    scale_x_discrete(breaks=c(), expand=c(0, 0), name="") +
    scale_y_continuous(label_long("n_implementations"), expand=c(0, 0), breaks=seq_len(500)) +
    scale_fill_manual(values=component_category_colors) +
    coord_flip() +
    theme(legend.position="top")
implementation_components_ordering_plot

n_implementations <- length(unique(implementation_components$implementation_id))
category_counts <- implementation_components %>%
  left_join(component_categories, "component_id") %>%
  group_by(implementation_id, category) %>%
  summarise() %>%
  group_by(category) %>%
  count() %>%
  filter(!is.na(category))

implementation_components_categories_plot <- bind_rows(
  category_counts %>% mutate(included = category),
  category_counts %>% mutate(included = "unknown", n = n_implementations - n)
) %>%
  mutate(included = fct_inorder(included)) %>%
  ggplot(aes(1, n)) +
    geom_bar(aes(fill=included), stat="identity") +
    geom_text(aes(label=n, y=n_implementations - 4), fill=NA, category_counts, size=5) +
    facet_grid(.~category, labeller=label_facet()) +
    scale_fill_manual(values=component_category_colors %>% c(unknown="grey")) +
    coord_polar("y") +
    theme_void() +
    theme(legend.position="none")
implementation_components_categories_plot




##  ............................................................................
##  implementation components plot                                                  ####
implementation_components_plot <-
  cowplot::plot_grid(implementation_components_wordcloud_plot, implementation_components_ordering_plot, rel_widths = c(0.6, 0.4), labels="auto") %>%
  cowplot::plot_grid(., implementation_components_categories_plot, ncol=1, rel_heights = c(0.6, 0.4), labels = c("", "c"))
implementation_components_plot

save_plot(figure_file("implementation_components.svg"), implementation_components_plot, base_width=15, base_height=12)

