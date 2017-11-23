library(tidyverse)
library(cowplot)
library(googlesheets)
library(dynalysis)
# devtools::load_all()

dataset_infos <- gs_title("Real datasets") %>%
  gs_read(ws = "datasets", col_types = cols(date = col_date())) %>%
  separate_rows(id, sep=",")

dataset_ids <- dataset_infos$id

map_prism <- function (x, f) PRISM::qsub_lapply(x, f, qsub_environment = list2env(list()), qsub_config = PRISM::override_qsub_config(memory="4G"))
map_local <- function(x, f) parallel::mclapply(x, f, mc.cores=6)


# Check the datasets ------------------------------------
# dataset_checks <- map_prism(dataset_ids, function(dataset_id) {
dataset_checks <- map_local(dataset_ids, function(dataset_id) {
  library(tidyverse)
# dataset_checks <- parallel::mclapply(mc.cores=1, dataset_ids, function(dataset_id) {
  print(paste0("-------------", dataset_id))
  print("loading")
  dataset <- dynalysis::load_dataset(dataset_id, "real")
  dataset %>% list2env(.GlobalEnv)

  all_milestones_represented <- all(milestone_ids %in% cell_grouping$group_id)

  lst(all_milestones_represented, id=info$id) %>% as_tibble()
})
dataset_checks <- dataset_checks %>% bind_rows()

# Score the datasets ---------------------------------
# dataset_scores <- map_local(dataset_ids, function(dataset_id) {
dataset_scores <- map_prism(dataset_ids, function(dataset_id) {
  library(tidyverse)
  library(dynalysis)
  print(paste0("-------------", dataset_id))
  print("loading")
  dataset <- dynalysis::load_dataset(dataset_id, "real")
  dataset %>% list2env(.GlobalEnv)

  Coi <- rowMeans(dataset$expression) %>% {(. > (median(.) - IQR(., 0.5)*2)) & (. < (median(.) + IQR(., 0.5)*2))} %>% keep(~.) %>% names

  gene_sds <- apply(dataset$expression[Coi, ], 2, sd) %>% sort(decreasing = TRUE)
  Goi <- gene_sds %>% names %>% head(5000)

  if(length(Goi) == 0) {stop("No genes selected")}
  if(length(Coi) == 0) {stop("No cells selected")}
  if(length(Coi) > 500) {Coi=sample(Coi, 500)}

  Coi <- Coi[dataset$expression[Coi, Goi] %>% apply(1, sd) %>% {. > 0}]

  # make sure we select for every group at least one cell
  Coi2 <- dataset$cell_grouping %>% group_by(group_id) %>% filter(row_number() == 1) %>% pull(cell_id)
  Coi <- union(Coi, Coi2)

  expression_grouped <- dataset$expression[Coi, Goi] %>% dynutils::group_counts(dataset$cell_grouping)
  scores <- list()

  print("transitions")
  scores <- bind_cols(
    scores,
    score_milestone_transitions(expression_grouped, dataset$milestone_ids, dataset$milestone_network)
  )
  print("grouping")
  scores <- bind_cols(
    scores,
    score_milestone_grouping(dataset$expression[Coi, Goi], dataset$cell_grouping)
  )
  print("connectivity")
  scores <- bind_cols(
    scores,
    score_milestone_connectivity(dataset$expression[Coi, Goi], dataset$cell_grouping, dataset$milestone_ids, dataset$milestone_network)
  )
  print("aggregation")
  scores <- bind_cols(
    scores,
    score_aggregation(dataset$expression[Coi, Goi], dataset$cell_grouping)
  )

  scores$dataset_id <- dataset$info$id

  scores
})
dataset_scores <- dataset_scores %>% bind_rows()

score_ids <- c("aggregation_sd_frac", "transition_pval", "transition_frac", "grouping_asw", "connectivity")
score_baselines <- tribble(
  ~score_id, ~baseline, ~type,
  "aggregation_sd_frac", 0.8, "hard",
  "transition_pval", -log10(0.1), "hard",
  "transition_frac", 1.5, "soft",
  "transition_frac", 1, "hard",
  "grouping_asw", 0, "hard",
  "connectivity", 0.2, "soft"
)
dataset_scores %>% .[c(score_ids[score_ids %in% colnames(dataset_scores)], "dataset_id")] %>% gather("score_id", "score", -dataset_id) %>%
  ggplot() +
  geom_bar(aes(dataset_id, score), stat="identity") +
  facet_wrap(~score_id, scales = "free_x", ncol=10) +
  geom_point(aes(dataset_id, 0, alpha=is.na(score))) +
  geom_hline(aes(yintercept=baseline, color=type), data=score_baselines) +
  coord_flip()



### Cell plotting ----------------
cell_plots <- map_prism(dataset_ids, function(dataset_id) {
# cell_plots <- map_local(dataset_ids, function(dataset_id) {
  library(tidyverse)
  plots <- list()

  dataset <- dynalysis::load_dataset(dataset_id, "real")

  gene_sds <- apply(dataset$expression, 2, sd) %>% sort(decreasing = TRUE)
  Goi <- gene_sds %>% head(5000) %>% names()
  Coi <- sample(rownames(dataset$expression), min(nrow(dataset$expression), 500))
  Coi <- Coi[dataset$expression[Coi, Goi] %>% apply(1, sd) %>% {. > 0}]

  dimred_mds = function(x, ndim=3) {
    space = SCORPIUS::reduce_dimensionality(SCORPIUS::correlation_distance(x),ndim = ndim)
    space = as.matrix(space)
    colnames(space) = paste0("Comp", 1:ncol(space))
    space
  }

  space = dimred_mds(dataset$expression[Coi, Goi], ndim = 2)

  plotdata_cells <- bind_cols(space %>% as.data.frame() %>% tibble::rownames_to_column("cell_id")) %>% left_join(dataset$cell_info, by="cell_id") %>% left_join(dataset$cell_grouping, by="cell_id")
  plotdata_cells$ncount <- rowMeans(dataset$expression[Coi, Goi])

  plotdata_groups <- plotdata_cells %>% group_by(group_id) %>% summarise(Comp1=mean(Comp1), Comp2=mean(Comp2))

  plotdata_network <- dataset$milestone_network %>%
    left_join(plotdata_groups %>% rename_at(vars(starts_with("Comp")), list(~paste0("from_", .))), by=c("from"="group_id")) %>%
    left_join(plotdata_groups %>% rename_at(vars(starts_with("Comp")), list(~paste0("to_", .))), by=c("to"="group_id"))

  plots$plot_cells <- list(
    ggplot() +
    geom_point(aes(Comp1, Comp2, color=group_id), shape=21, stroke=4, data=plotdata_groups, size=3, fill="black") +
    geom_segment(aes(from_Comp1, from_Comp2, xend=to_Comp1, yend=to_Comp2), data=plotdata_network, arrow=arrow(), size=2, alpha=0.2) +
    geom_point(aes(Comp1, Comp2, color=group_id), data=plotdata_cells) +
    theme(legend.position="bottom")
  )

  plots$plot_ncount <- list(
    ggplot() +
    geom_point(aes(Comp1, Comp2), shape=21, stroke=4, data=plotdata_groups, size=3, alpha=0.3) +
    geom_segment(aes(from_Comp1, from_Comp2, xend=to_Comp1, yend=to_Comp2), data=plotdata_network, arrow=arrow(), size=2, alpha=0.2) +
    geom_point(aes(Comp1, Comp2, color=ncount), data=plotdata_cells) +
    viridis::scale_color_viridis()
  )

  rm(dataset)

  plots
})
cell_plots <- cell_plots %>% bind_rows()

grid <- cowplot::plot_grid(plotlist=cell_plots %>% pull(plot_cells))
grid


### Network plotting --------------------------
library(ggnetwork)
network_plots <- parallel::mclapply(dataset_ids, function(dataset_id) {
  library(tidyverse)
  print(paste0("-------------", dataset_id))
  print("loading")
  dataset <- dynalysis::load_dataset(dataset_id, "real")

  milestone_network <- dataset$milestone_network
  milestone_nodes <- dataset$cell_info %>% group_by(milestone_id) %>% summarise(n=n())

  graph <- igraph::graph_from_data_frame(milestone_network, TRUE, milestone_nodes)

  fortified <- fortify(graph, arrow.gap=0)

  base_plot <- fortified %>%
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    theme_blank() +
    scale_x_continuous(expand=c(0.2, 0)) +
    scale_y_continuous(expand=c(0.2, 0)) +
    theme(legend.position="none")

  network_plot_labeled <- list(
    base_plot +
      geom_edges() +
      geom_edges(aes(x=x, y=y, xend=xend - (xend - x)/3, yend = yend - (yend - y)/3), arrow=arrow(type="closed", length=unit(0.1, "inches"))) +
      geom_edges(aes(x=x, y=y, xend=xend - (xend - x)/3*2, yend = yend - (yend - y)/3*2), arrow=arrow(type="closed", length=unit(0.1, "inches"))) +
      geom_nodelabel(aes(label=vertex.names, fill=vertex.names), color="white")
  )

  network_plot = list(
    base_plot +
      geom_edges() +
      geom_edges(aes(x=x, y=y, xend=xend - (xend - x)/3, yend = yend - (yend - y)/3), arrow=arrow(type="closed", length=unit(0.1, "inches"))) +
      geom_edges(aes(x=x, y=y, xend=xend - (xend - x)/3*2, yend = yend - (yend - y)/3*2), arrow=arrow(type="closed", length=unit(0.1, "inches"))) +
      geom_nodes(aes(color=vertex.names), size=10)
  )

  network_plot_sizes = list(
    base_plot +
      geom_edges() +
      geom_edges(aes(x=x, y=y, xend=xend - (xend - x)/3, yend = yend - (yend - y)/3), arrow=arrow(type="closed", length=unit(0.1, "inches"))) +
      geom_edges(aes(x=x, y=y, xend=xend - (xend - x)/3*2, yend = yend - (yend - y)/3*2), arrow=arrow(type="closed", length=unit(0.1, "inches"))) +
      geom_nodes(aes(color=vertex.names, size=n)) +
      scale_size_continuous(range=c(3, 20), trans="log10", limits=c(1, 100000))
  )

  lst(
    id = dataset$info$id,
    network_plot_labeled,
    network_plot
  ) %>% as_tibble()
}, mc.cores=8)
network_plots <- network_plots %>% bind_rows()

library(ggnetwork)
network_characteristics <- parallel::mclapply(dataset_ids, function(dataset_id) {
  library(tidyverse)
  print(paste0("-------------", dataset_id))
  print("loading")
  dataset <- load_dataset(dataset_id)

  contains_cycle <- FALSE
  tryCatch(
    {igraph::graph_from_data_frame(dataset$milestone_network) %>% igraph::topo_sort()}
    , warning=function(w) contains_cycle <<- TRUE
  )

  contains_bifurcating <- dataset$milestone_network$from %>% table %>% {any(.>1)}
  contains_convergence <- dataset$milestone_network$to %>% table %>% {any(.>1)}

  lst(contains_cycle, contains_bifurcating, contains_convergence, id=dataset$info$id)
}, mc.cores=8) %>% bind_rows()
network_characteristics$topology_id <- network_characteristics %>% select_if(is.logical) %>% apply(1, function(x) sum((2^seq_along(x))[x]))


datasets <- dataset_infos %>% left_join(network_characteristics, "id") %>% left_join(network_plots, "id")

grid <- cowplot::plot_grid(plotlist=datasets %>% arrange(topology_id) %>% filter(standard=="gold") %>% pull(network_plot))
title <- ggdraw() + draw_label("Gold standard networks", fontface='bold')
plot_grid(title, grid, ncol=1, rel_heights=c(0.1, 1)) # rel_heights values control title margins


grid <- cowplot::plot_grid(plotlist=datasets %>% arrange(topology_id) %>% filter(standard=="silver") %>% pull(network_plot))
title <- ggdraw() + draw_label("Silver standard networks", fontface='bold')
plot_grid(title, grid, ncol=1, rel_heights=c(0.1, 1)) # rel_heights values control title margins
