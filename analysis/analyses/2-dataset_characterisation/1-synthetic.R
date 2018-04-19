library(dynalysis)
library(tidyverse)

library(tidygraph)
library(ggraph)

experiment("2-dataset_characterisation/1-synthetic")
tasks <- load_datasets_tibble()


##  ............................................................................
##  Modulenet graphs                                                        ####
modulenet_names <- tasks$settings %>% map("modulenet_name") %>% unlist() %>% unique()

params <- simple_params

scale_edge_burn <- scale_edge_linetype_manual(values=c(`FALSE` = "solid", `TRUE` = "dashed"))
theme_without_legend <- theme(legend.position = "none")

plots <- map(modulenet_names, function(modulenet_name) {
  model <- dyngen:::load_modulenet(modulenet_name)

  # Module net graph
  modulenet_graph <- tbl_graph(model$modulenodes, model$modulenet)%>%
    activate(edges) %>%
    mutate(effect = as.character(effect)) %>%
    activate(nodes) %>%
    mutate(a0 = as.character(a0))


  modulenet_plot <- modulenet_graph %>%
    activate(edges) %>%
    arrange(from == to) %>%
    ggraph(layout = 'linear', circular = TRUE) +
    geom_edge_link(aes(color = effect), arrow=arrow(type="closed"), end_cap = circle(3, 'mm')) +
    geom_edge_loop(aes(color = effect), arrow=arrow(type="closed"), end_cap = circle(3, 'mm')) +
    geom_node_label(aes(fill = a0, label=module_id)) +
    scale_edge_color_manual(values = c("-1"="#0074D9", "1"="#FF4136")) +
    scale_fill_manual(values = c("1"="#FF4136", "0"="white")) +
    theme_graph() +
    theme_without_legend
  modulenet_plot

  # Milestone graph
  edge_operations <- model$edge_operations %>%
    mutate(edge_id = factor(seq_len(n())))

  milestone_graph <- edge_operations %>%
    as_tbl_graph()

  milestone_plot <- ggraph(milestone_graph, layout = "fr") +
    geom_node_point(aes()) +
    geom_edge_link(aes(color = edge_id, edge_linetype=burn), arrow=arrow(type="closed")) +
    scale_edge_burn +
    theme_graph("") +
    theme_without_legend +
    ggtitle(label_long(modulenet_name))
  milestone_plot

  # Edge operations graph
  edge_operations_graph <- edge_operations %>%
    separate_rows(module_progression, sep="\\|") %>%
    group_by(from, to) %>%
    mutate(
      from_b = ifelse(row_number() == 1, as.character(from), paste0(from, "_", edge_id, "_", seq_len(n())-1)),
      to_b = ifelse(row_number() == n(), as.character(to), paste0(from, "_", edge_id, "_", seq_len(n())))
    ) %>%
    ungroup() %>%
    select(from_b, to_b, module_progression, burn, edge_id) %>%
    as_tbl_graph()

  edge_operations_plot <- ggraph(edge_operations_graph, layout = "fr") +
    geom_edge_link(aes(label = module_progression, edge_linetype=burn, color=edge_id), arrow=arrow(type="closed"), angle_calc = 'along', label_dodge = unit(2.5, 'mm')) +
    geom_node_point() +
    theme_graph() +
    theme_without_legend
  edge_operations_plot

  lst(modulenet_plot, milestone_plot, edge_operations_plot)
})

modulenet_plots <- map(plots, ~cowplot::plot_grid(plotlist=., ncol=3, align = "v"))

ggrid <- cowplot::plot_grid(plotlist = modulenet_plots, ncol = 1)
ggsave(figure_file("modulenets.svg"), ggrid, width = 12, height = 35)



##  ............................................................................
##  Module net to gene net example                                          ####
model <- ""





##  ............................................................................
##  Samplers table                                                          ####
samplers <- read_tsv(raw_file("samplers"))
notes <- c("$y_{max} = r/d * p/q$")

table <- map(c("latex", "html"), function(format) {
  table <- samplers %>% mutate(text = pritt("${parameter} = {distribution}$")) %>%
    select(text) %>%
    mutate(text = kableExtra::cell_spec(text, format, escape=FALSE)) %>%
    knitr::kable(format, escape = FALSE, col.names=NULL) %>%
    kableExtra::kable_styling(bootstrap_options="condensed") %>%
    kableExtra::footnote(notes, general_title = "where") %>%
    gsub("&amp;", "&", .)
  table
}) %>% set_names(c("latex", "html"))

table %>% saveRDS(figure_file("samplers.rds"))
