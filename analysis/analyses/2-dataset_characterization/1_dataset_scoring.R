library(tidyverse)
library(cowplot)
library(googlesheets)
library(dynalysis)
# devtools::load_all()

task_infos <- gs_title("Real datasets") %>%
  gs_read(ws = "datasets", col_types = cols(date = col_date())) %>%
  separate_rows(id, sep=",\n") %>%
  mutate(id = paste0("real/", id))

task_ids <- task_infos$id

# tasks not online but found physically
paste0("real/", list.files("analysis/data/derived_data/datasets/real/"))[!(paste0("real/", list.files("analysis/data/derived_data/datasets/real/")) %in% task_ids)]

task_ids[!(task_ids %in% paste0("real/", list.files("analysis/data/derived_data/datasets/real/")))]
task_ids <- task_ids[(task_ids %in% paste0("real/", list.files("analysis/data/derived_data/datasets/real/")))]

map_prism <- function (x, f) PRISM::qsub_lapply(x, f, qsub_environment = list2env(list()), qsub_config = PRISM::override_qsub_config(memory="4G"))
map_local <- function(x, f) pbapply::pblapply(x, f, cl=1)
map_local_parallel <- function(x, f) pbapply::pblapply(x, f, cl=8)

tasks_synthetic <- read_rds("analysis/data/derived_data/datasets/synthetic/v5.rds")
tasks_real <- map_local(task_ids, function(task_id) {
  print(task_id)
  task <- readRDS(dataset_file("dataset.rds", dataset_id = task_id))

  print(task$trajectory_type)

  task$expression <- function() {readRDS(dataset_file("dataset.rds", dataset_id = task_id))$expression}
  task$counts <- function() {readRDS(dataset_file("dataset.rds", dataset_id = task_id))$counts}

  dynutils::list_as_tibble(list(task))
})
tasks_toy <- dyntoy::toy_tasks
tasks <- bind_rows(tasks_synthetic, tasks_real, toy_tasks)

tasks$category <- ifelse(tasks$id %>% startsWith("real"), "real", ifelse(tasks$id %>% startsWith("toy"), "toy", "synthetic"))


# Check the tasks ------------------------------------
# task_checks <- map_prism(task_ids, function(task_id) {
task_checks <- pmap(as.list(tasks), function(...) {
  # task <- extract_row_to_list(tasks, 1)
  task <- list(...)
  library(tidyverse)
  print(paste0("-------------", task$id))
  print("loading")

  all_milestones_represented <- all(task$milestone_ids %in% task$prior_information$grouping_assignment$group_id)

  lst(all_milestones_represented, task_id = task$id, n_cells = nrow(task$cell_info), n_genes = nrow(task$feature_info)) %>% as_tibble()
}) %>% bind_rows()

task_checks %>% bind_cols(tasks) %>% ggplot() + geom_point(aes(n_genes, n_cells, color=category))

task_ids_filtered <- task_checks %>% filter(all_milestones_represented) %>% pull(task_id)
tasks_filtered <- tasks %>% filter(id %in% task_ids_filtered)

load_expression <- function(expr) {
  if(class(expr) == "function") {
    expr()
  } else {
    expr
  }
}

# Score the tasks ---------------------------------
task_scores <- pmap(as.list(tasks_filtered), function(...) {
  # task <- extract_row_to_list(tasks, 20)
  task <- list(...)
  library(tidyverse)
  print(paste0("-------------", task$id))
  print("loading")

  task$expression <- load_expression(task$expression)

  Coi <- rowMeans(task$expression) %>% {(. > (median(.) - IQR(., 0.5)*2)) & (. < (median(.) + IQR(., 0.5)*2))} %>% keep(~.) %>% names

  gene_sds <- apply(task$expression[Coi, ], 2, sd) %>% sort(decreasing = TRUE)
  Goi <- gene_sds %>% names %>% head(5000)

  if(length(Goi) == 0) {stop("No genes selected")}
  if(length(Coi) == 0) {stop("No cells selected")}
  if(length(Coi) > 500) {Coi=sample(Coi, 500)}

  Coi <- Coi[task$expression[Coi, Goi] %>% apply(1, sd) %>% {. > 0}]

  # make sure we select for every group at least one cell
  Coi2 <- task$prior_information$grouping_assignment %>% group_by(group_id) %>% filter(row_number() == 1) %>% pull(cell_id)
  Coi <- union(Coi, Coi2)

  expression_grouped <- task$expression[Coi, Goi] %>% dynutils::group_counts(task$prior_information$grouping_assignment)
  scores <- list()

  print("transitions")
  scores <- bind_cols(
    scores,
    score_milestone_transitions(expression_grouped, task$milestone_ids, task$milestone_network)
  )
  print("grouping")
  scores <- bind_cols(
    scores,
    score_milestone_grouping(task$expression[Coi, Goi], task$prior_information$grouping_assignment)
  )
  print("connectivity")
  scores <- bind_cols(
    scores,
    score_milestone_connectivity(task$expression[Coi, Goi], task$prior_information$grouping_assignment, task$milestone_ids, task$milestone_network)
  )
  print("aggregation")
  scores <- bind_cols(
    scores,
    score_aggregation(task$expression[Coi, Goi], task$prior_information$grouping_assignment)
  )

  scores$task_id <- task$id

  scores
})
task_scores <- task_scores %>% bind_rows()

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
task_scores %>% .[c(score_ids[score_ids %in% colnames(task_scores)], "task_id")] %>% gather("score_id", "score", -task_id) %>%
  ggplot() +
  geom_bar(aes(task_id, score), stat="identity") +
  facet_wrap(~score_id, scales = "free_x", ncol=10) +
  geom_point(aes(task_id, 0, alpha=is.na(score))) +
  geom_hline(aes(yintercept=baseline, color=type), data=score_baselines) +
  coord_flip()



### Cell plotting ----------------
cell_plots <- pmap(as.list(tasks_filtered), function(...) {
  # task <- extract_row_to_list(tasks, 20)
  task <- list(...)
  library(tidyverse)
  print(paste0("-------------", task$id))
  print("loading")

  task$expression <- load_expression(task$expression)

  source("../dynmodular/dimred_wrappers.R")

  plots <- list()

  space <- dimred_mds(task$expression)

  plotdata_cells <- bind_cols(space %>% as.data.frame() %>% tibble::rownames_to_column("cell_id")) %>% left_join(task$cell_info, by="cell_id") %>% left_join(task$prior_information$grouping_assignment, by="cell_id")
  plotdata_cells$libsize <- rowMeans(task$expression)

  plotdata_groups <- plotdata_cells %>% group_by(group_id) %>% summarise(Comp1=mean(Comp1), Comp2=mean(Comp2))

  plotdata_network <- task$milestone_network %>%
    left_join(plotdata_groups %>% rename_at(vars(starts_with("Comp")), list(~paste0("from_", .))), by=c("from"="group_id")) %>%
    left_join(plotdata_groups %>% rename_at(vars(starts_with("Comp")), list(~paste0("to_", .))), by=c("to"="group_id"))

  base_scater <- ggplot(plotdata_groups, aes(Comp1, Comp2)) +
    geom_point(aes(Comp1, Comp2), shape=21, stroke=4, size=3, alpha=0.3) +
    geom_segment(aes(from_Comp1, from_Comp2, xend=to_Comp1, yend=to_Comp2), data=plotdata_network, arrow=arrow(), size=2, alpha=0.2) +
    geom_point(aes(Comp1, Comp2, fill=group_id), shape=23, stroke=1, data=plotdata_groups, size=3, color="black") +
    theme(legend.position="bottom")

  plots$plot_cells <- list(
    base_scater + geom_point(aes(Comp1, Comp2, color=group_id), data=plotdata_cells)
  )
  plots$plot_libsize <- list(
    base_scater +
      geom_point(aes(Comp1, Comp2, color=libsize), data=plotdata_cells) +
      viridis::scale_color_viridis(option="A")
  )

  rm(task)

  plots
})
cell_plots <- cell_plots %>% bind_rows()

grid <- cowplot::plot_grid(plotlist=cell_plots %>% pull(plot_cells))
grid

save_plot("grid.pdf", grid, base_height=30, base_width=30)

grid <- cowplot::plot_grid(plotlist=cell_plots %>% pull(plot_libsize))
save_plot("grid2.pdf", grid, base_height=30, base_width=30)

s <- pmap(as.list(tasks_filtered), function(...) {
  # task <- extract_row_to_list(tasks, 20)
  task <- list(...)
  library(tidyverse)
  print(paste0("-------------", task$id))
  print("loading")

  task$expression <- load_expression(task$expression)

  source("../dynmodular/dimred_wrappers.R")

  space <- dimred_mds(task$expression)

  plotdata_cells <- bind_cols(space %>% as.data.frame() %>% tibble::rownames_to_column("cell_id")) %>% left_join(task$cell_info, by="cell_id") %>% left_join(task$prior_information$grouping_assignment, by="cell_id")
  plotdata_cells$libsize <- rowMeans(task$expression)

  plotdata_groups <- plotdata_cells %>% group_by(group_id) %>% summarise(Comp1=mean(Comp1), Comp2=mean(Comp2))

  plotdata_network <- task$milestone_network %>%
    left_join(plotdata_groups %>% rename_at(vars(starts_with("Comp")), list(~paste0("from_", .))), by=c("from"="group_id")) %>%
    left_join(plotdata_groups %>% rename_at(vars(starts_with("Comp")), list(~paste0("to_", .))), by=c("to"="group_id"))

  base_scater <- ggplot(plotdata_groups, aes(Comp1, Comp2)) +
    geom_point(aes(Comp1, Comp2), shape=21, stroke=4, size=3, alpha=0.3) +
    geom_segment(aes(from_Comp1, from_Comp2, xend=to_Comp1, yend=to_Comp2), data=plotdata_network, arrow=arrow(), size=2, alpha=0.2) +
    geom_point(aes(Comp1, Comp2, fill=group_id), shape=23, stroke=1, data=plotdata_groups, size=3, color="black") +
    theme(legend.position="bottom")

  plots$plot_cells <- list(
    base_scater + geom_point(aes(Comp1, Comp2, color=group_id), data=plotdata_cells)
  )
  plots$plot_libsize <- list(
    base_scater +
      geom_point(aes(Comp1, Comp2, color=libsize), data=plotdata_cells) +
      viridis::scale_color_viridis(option="A")
  )

  rm(task)

  plots


### Network plotting --------------------------
library(ggnetwork)
network_plots <- map_local(task_ids_filtered, function(task_id) {
  library(tidyverse)
  print(paste0("-------------", task_id))
  print("loading")
  task <- readRDS(dataset_file("dataset.rds", dataset_id = task_id))

  milestone_network <- task$milestone_network
  milestone_nodes <- task$cell_info %>% group_by(milestone_id) %>% summarise(n=n())

  end_nodes <- (milestone_nodes$milestone_id %in% milestone_network$to) & !(milestone_nodes$milestone_id %in% milestone_network$from)
  start_nodes <- !(milestone_nodes$milestone_id %in% milestone_network$to) & (milestone_nodes$milestone_id %in% milestone_network$from)
  milestone_nodes$type <- ifelse(start_nodes, "start", ifelse(end_nodes, "end", "inner"))
  type_shapes <- c("start"=17, "inner"=16, "end"=15)
  type_colors <- c("start"="#0074D9", "inner"="#FF851B", "end"="#FF4136")

  graph <- igraph::graph_from_data_frame(milestone_network, TRUE, milestone_nodes)

  fortified <- fortify(graph, arrow.gap=0)

  base_plot <- fortified %>%
    ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
    theme_blank() +
    scale_x_continuous(expand=c(0.2, 0)) +
    scale_y_continuous(expand=c(0.2, 0)) +
    theme(legend.position="none") +
    geom_edges() +
    geom_edges(aes(x=x, y=y, xend=xend - (xend - x)/3, yend = yend - (yend - y)/3), arrow=arrow(type="closed", length=unit(0.1, "inches"))) +
    geom_edges(aes(x=x, y=y, xend=xend - (xend - x)/3*2, yend = yend - (yend - y)/3*2), arrow=arrow(type="closed", length=unit(0.1, "inches")))

  network_plot_labeled <- list(
    base_plot +
      geom_nodelabel(aes(label=vertex.names, fill=vertex.names), color="white")
  )

  network_plot = list(
    base_plot +
      geom_nodes(aes(color=vertex.names, shape = type), size=10) + scale_shape_manual(values=type_shapes)
  )

  network_plot_startstop = list(
    base_plot +
      geom_nodes(aes(color=type, shape = type), size=10) +
      scale_shape_manual(values=type_shapes) +
      scale_color_manual(values=type_colors)
  )

  network_plot_sizes = list(
    base_plot +
      geom_nodes(aes(color=vertex.names, size=n)) +
      scale_size_continuous(range=c(3, 20), trans="log10", limits=c(1, 100000))
  )

  lst(
    id = task_id,
    network_plot_labeled,
    network_plot,
    network_plot_sizes,
    network_plot_startstop
  ) %>% as_tibble()
})
network_plots <- network_plots %>% bind_rows()

library(ggnetwork)
network_characteristics <- parallel::mclapply(task_ids, function(task_id) {
  library(tidyverse)
  print(paste0("-------------", task_id))
  print("loading")
  task <- readRDS(dataset_file("dataset.rds", dataset_id = task_id))

  contains_cycle <- FALSE
  tryCatch(
    {igraph::graph_from_data_frame(task$milestone_network) %>% igraph::topo_sort()}
    , warning=function(w) contains_cycle <<- TRUE
  )

  contains_bifurcating <- task$milestone_network$from %>% table %>% {any(.>1)}
  contains_convergence <- task$milestone_network$to %>% table %>% {any(.>1)}

  lst(contains_cycle, contains_bifurcating, contains_convergence, id=task_id)
}, mc.cores=8) %>% bind_rows()
network_characteristics$topology_id <- network_characteristics %>% select_if(is.logical) %>% apply(1, function(x) sum((2^seq_along(x))[x]))


tasks <- task_infos %>% left_join(network_characteristics, "id") %>% left_join(network_plots, "id")

grid <- cowplot::plot_grid(plotlist=tasks %>% arrange(topology_id) %>% filter(standard=="gold") %>% pull(network_plot_startstop))
title <- ggdraw() + draw_label("Gold standard networks", fontface='bold')
plot_grid(title, grid, ncol=1, rel_heights=c(0.1, 1)) # rel_heights values control title margins


grid <- cowplot::plot_grid(plotlist=tasks %>% arrange(topology_id) %>% filter(standard=="silver") %>% pull(network_plot))
title <- ggdraw() + draw_label("Silver standard networks", fontface='bold')
plot_grid(title, grid, ncol=1, rel_heights=c(0.1, 1)) # rel_heights values control title margins
