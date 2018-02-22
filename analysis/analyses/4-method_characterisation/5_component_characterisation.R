library(tidyverse)
library(dynalysis)
library(cowplot)
library(tidytext)

experiment("4-method_characterisation")

methods <- read_rds(derived_file("methods.rds"))

<<<<<<< HEAD
method_components <- methods %>% select(method_id, components) %>%
=======
method_components <- methods %>% select(method_ids, components) %>%
>>>>>>> 9e301137201def4def1a1fd2c24b1386d3004001
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
  "GPLVM", "dimensionality reduction",
  "GPLVM", "ordering",
  "any multiple dimensionality reductions", "dimensionality reduction",
  "any dimensionality reduction", "dimensionality reduction",
  "diffusion map", "dimensionality reduction",
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
  "laplacian eigenmap", "dimensionality reduction",
  "EM clustering", "clustering",
  "consensus clustering", "clustering",
  "weighted cluster graphs", "graph building",
  "maximum likelihood ordering", "ordering"
) %>% group_by(component_id) %>%
  filter(row_number() == 1)

write_rds(method_components, derived_file("method_components.rds"))
write_rds(component_categories, derived_file("component_categories.rds"))

method_component_counts <- method_components %>%
  count(component_id) %>%
  left_join(component_categories, "component_id")


method_components_wordcloud_plot <- method_component_counts %>%
  filter(n > 1 | !is.na(category)) %>%
  ggplot() +
    ggrepel::geom_text_repel(aes(1, 1, size=n, label=component_id, color=category), segment.size = 0) +
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
  mutate(component_id = forcats::fct_inorder(component_id)) %>%
  ggplot(aes(component_id, n, fill = category)) +
    geom_bar(stat="identity") +
    geom_text(aes(y=0, label=pritt("{component_id}: {n}")), stat="identity", hjust=0) +
    scale_x_discrete(breaks=c(), expand=c(0, 0), name="") +
    scale_y_continuous(expand=c(0, 0), name="# methods") +
    coord_flip() +
    theme(legend.position="top")
method_components_ordering_plot

<<<<<<< HEAD
n_methods <- length(unique(method_components$method_id))
category_counts <- method_components %>%
  left_join(component_categories, "component_id") %>%
  group_by(method_id, category) %>%
=======
n_methods <- length(unique(method_components$method_ids))
category_counts <- method_components %>%
  left_join(categories, "component") %>%
  group_by(method_ids, category) %>%
>>>>>>> 9e301137201def4def1a1fd2c24b1386d3004001
  summarise() %>%
  group_by(category) %>%
  count() %>%
  filter(!is.na(category))

method_components_categories_plot <- bind_rows(
  category_counts %>% mutate(included = category),
  category_counts %>% mutate(included = "not", n = n_methods - n)
) %>%
  ggplot(aes(1, n, fill=included)) +
    geom_bar(stat="identity") +
    facet_wrap(~category, labeller=label_facet()) +
    coord_polar("y")
method_components_categories_plot




##  ............................................................................
##  Method components plot                                                  ####
method_components_plot <- cowplot::plot_grid(method_components_wordcloud_plot, method_components_ordering_plot)
method_components_plot

saveRDS(method_components_plot, figure_file("method_components.rds"))

