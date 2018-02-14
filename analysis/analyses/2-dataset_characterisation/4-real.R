library(tidyverse)
library(cowplot)
library(googlesheets)
library(dynalysis)


experiment("2-dataset_characterisation")

tasks <- readRDS(derived_file("tasks.rds"))
tasks_real <- tasks %>% filter(category == "real")

technology_colors <- unique(tasks_real$technology) %>% {setNames(RColorBrewer::brewer.pal(length(.), "Set1"), .)}
standard_colors <- c("gold"="#ffeca9", "silver"="#e3e3e3")

##  ............................................................................
##  Pie charts                                                              ####
pie <- function(tasks=tasks_real, what = "technology") {
  tasks$variable_of_interest <- tasks[[what]]

  # first extract the variable
  pie_data <- tasks %>%
    count(variable_of_interest) %>%
    arrange(n)

  # change labels, fill or order depending on variable
  label <-"{label_short(variable_of_interest, 15)}: {n}"
  fill_scale <- scale_fill_grey(start = 0.6, end = 0.9)
  text_position <- 0.5
  if (what == "standard") {
    fill_scale <- scale_fill_manual(values=standard_colors)
  } else if (what == "technology") {
    fill_scale <- scale_fill_manual(values=technology_colors)
  } else if (what == "trajectory_type") {
    fill_scale <- scale_fill_manual(values=setNames(trajectory_types$background_color,trajectory_types$id))
    pie_data <- pie_data %>% arrange(
      -match(as.character(variable_of_interest), trajectory_types$id)
    )
    label <- "{n}"
    text_position <- 0.9
  }

  # now calcualte positions
  pie_data <- pie_data %>% mutate(odd = row_number()%%2, row_number = row_number()) %>%
    mutate(start = lag(cumsum(n), 1, 0), end = cumsum(n), mid = start + (end - start)/2)

  pie <- pie_data %>%
    ggplot(aes(ymin=0, ymax=1)) +
    geom_rect(aes(xmin=start, xmax=end, fill=variable_of_interest), stat="identity", color="white", size=0.5) +
    geom_text(aes(mid, text_position, label = pritt(label), vjust=0.5), color="black", hjust=0.5, size=5) +
    fill_scale +
    ggtitle(label_long(what)) +
    theme_void() +
    theme(legend.position = "none") +
    scale_x_continuous(expand=c(0, 0)) +
    scale_y_continuous(expand=c(0, 0)) +
    coord_flip()
}

dataset_characterisation_pies <- c("technology", "organism", "standard","dynamic_process", "trajectory_type") %>% map(pie, tasks = tasks_real) %>% cowplot::plot_grid(plotlist=., nrow =1)
dataset_characterisation_pies




##  ............................................................................
##  Distributions                                                           ####
tasks_real$ngenes <- map_int(tasks_real$normalisation_info, ~last(.$normalisation_steps[["ngenes"]]))
tasks_real$ncells <- map_int(tasks_real$normalisation_info, ~last(.$normalisation_steps[["ncells"]]))
dataset_characterisation_distributions <- c("ncells", "ngenes", "date") %>% map(function(what) {
  nbins <- 50

  if (what %in% c("ncells", "ngenes")) {
    limits <- c(10, 30000)
    x_scale <- scale_x_log10(label_long(what), breaks=c(10, 100, 100, 1000, 10000), limits=limits)
    limits <- log10(limits)
  } else {
    limits <- c(as.Date("2014-01-01"), as.Date("2018-06-01"))
    x_scale <- scale_x_date(label_long(what), limits = limits, date_breaks="1 year", date_labels="%Y")
  }
  tasks_real$what <- tasks_real[[what]]

  tasks_real %>%
    ggplot(aes(what, fill=technology, color=technology)) +
    geom_dotplot(binwidth=as.numeric(max(limits) - min(limits)) / nbins, method = "histodot", binpositions="all", stackgroups=TRUE, stackratio=1.5, dotsize=0.7) +
    x_scale +
    scale_y_continuous(NULL, breaks=NULL, expand=c(0.02, 0)) +
    scale_fill_manual(values=technology_colors) +
    scale_color_manual(values=technology_colors) +
    theme(legend.position="none")
}) %>% cowplot::plot_grid(plotlist=., ncol=1)
dataset_characterisation_distributions



##  ............................................................................
##  Small characterisation figure                                           ####
dataset_characterisation_plot <- cowplot::plot_grid(
  dataset_characterisation_distributions,
  dataset_characterisation_pies,
  rel_heights = c(1,1.5),
  ncol=1
)
dataset_characterisation_plot


cowplot::save_plot(figure_file("dataset_characterisation.svg"), dataset_characterisation_plot, base_height = 12, base_width = 8)



##  ............................................................................
##  Datasets table                                                          ####
grouper <- function(x) paste0("<span style='display: none;'>{gse}</span> {", x, "}")
library(kableExtra)
table <- tasks_real %>%
  select(date, gse, organism, technology, id, trajectory_type, standard, standard_determination, ncells, ngenes) %>%
  arrange(date) %>%
  mutate(
    date = cell_spec(
      glue::glue(grouper("date")),
      escape=FALSE
    ),
    technology = cell_spec(
      glue::glue(grouper("technology")),
      background=technology_colors[technology],
      color="white",
      escape=FALSE
    ),
    organism = cell_spec(
      glue::glue(grouper("organism")),
      escape=FALSE
    ),
    trajectory_type = kableExtra::cell_spec(label_long(trajectory_type), background=set_names(trajectory_types$color, trajectory_types$id)[trajectory_type], color="white"),
    ncells = cell_spec(
      ncells,
      background=spec_color(log(ncells), end = 0.9, option = "D", direction = -1),
      escape=FALSE,
      color="white"
    ),
    ngenes = cell_spec(
      ngenes,
      background=spec_color(log(ngenes), end = 0.9, option = "D", direction = -1),
      escape=FALSE,
      color="white"
    ),
    standard_determination = cell_spec(
      standard_determination,
      background=standard_colors[standard]
    )
  ) %>%
  select(-id, -standard) %>%
  rename_all(label_long) %>%
  knitr::kable("html", escape=FALSE) %>%
  collapse_rows(1:4) %>%
  kable_styling(bootstrap_options = c("hover", "striped"))
table

table %>%
  write_rds(figure_file("datasets.rds"))


##  ............................................................................
##  Plots of each dataset                                                   ####
library(tidygraph)
library(ggraph)
plots <- seq_len(nrow(tasks_real)) %>% map(function(task_i) {
  task <- dynutils::extract_row_to_list(tasks_real, task_i)

  milestone_graph <- tbl_graph(tibble(node=factor(task$milestone_ids, levels=task$milestone_ids)), task$milestone_network)

  # determine layout
  layout <- "tree"
  if (task$trajectory_type == "directed_cycle") {
    layout <- "circle"
  }

  milestone_graph %>%
    ggraph(layout=layout) +
      geom_edge_link() +
      geom_edge_link(aes(xend=x + (xend - x)/2, yend = y + (yend - y)/2), arrow=arrow(type="closed")) +
      geom_node_label(aes(label=node, fill=node)) +
      theme_graph() +
      theme(legend.position="none")
})
cowplot::plot_grid(plotlist=plots[1:20])
