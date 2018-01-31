library(tidyverse)
library(cowplot)
library(googlesheets)
library(dynalysis)


experiment("dataset_characterisation")

tasks <- readRDS(derived_file("tasks.rds"))
trajectory_types <- readRDS(derived_file("trajectory_types.rds"))
tasks_real <- tasks %>% filter(category == "real")

technology_colors <- unique(tasks_real$technology) %>% {setNames(RColorBrewer::brewer.pal(length(.), "Set1"), .)}
standard_colors <- c("gold"="#ffeca9", "silver"="#e3e3e3")

##  ............................................................................
##  Pie charts                                                              ####
pie <- function(tasks, what = "technology") {
  label <-"{label_short(variable_of_interest, 15)}: {n}"
  if (what == "standard") {
    fill_scale <- scale_fill_manual(values=standard_colors)
  } else if (what == "technology") {
    fill_scale <- scale_fill_manual(values=technology_colors)
  } else if (what == "trajectory_type") {
    fill_scale <- scale_fill_manual(values=setNames(trajectory_types$background_color,trajectory_types$id))
    label <- "{n}"
  } else {
    fill_scale <- scale_fill_grey(start = 0.6, end = 0.9)
  }

  tasks$variable_of_interest <- tasks[[what]]
  tasks %>%
    count(variable_of_interest) %>%
    arrange(n) %>%
    #arrange(rbind(seq_len(n()),rev(seq_len(n())))[seq_len(n())]) %>% # do largest - smallest - second largest - second smallest - ...
    mutate(odd = row_number()%%2, row_number = row_number()) %>%
    mutate(variable_of_interest = factor(variable_of_interest, levels=variable_of_interest)) %>%
    mutate(start = lag(cumsum(n), 1, 0), end = cumsum(n), mid = start + (end - start)/2) %>%
    ggplot(aes(ymin=0, ymax=1)) +
    geom_rect(aes(xmin=start, xmax=end, fill=variable_of_interest), stat="identity", color="white", size=0.5) +
    geom_text(aes(mid, 0.5, label = pritt(label), vjust=0.5), color="black", hjust=0.5, size=5) +
    fill_scale +
    ggtitle(label_long(what)) +
    theme_void() +
    theme(legend.position = "none") +
    scale_x_continuous(expand=c(0, 0)) +
    scale_y_continuous(expand=c(0, 0)) +
    coord_flip()
}

dataset_characterisation_pies <- c("technology", "organism", "standard", "trajectory_type") %>% map(pie, tasks = tasks_real) %>% cowplot::plot_grid(plotlist=., nrow =1)
dataset_characterisation_pies




##  ............................................................................
##  Distributions                                                           ####
tasks_real$ngenes <- map_int(tasks_real$normalisation_info, ~last(.$normalisation_steps[["ngenes"]]))
tasks_real$ncells <- map_int(tasks_real$normalisation_info, ~last(.$normalisation_steps[["ncells"]]))
dataset_characterisation_distributions <- c("ncells", "ngenes", "date") %>% map(function(what) {
  nbins <- 30

  if (what %in% c("ncells", "ngenes")) {
    limits <- c(10, 10000)
    x_scale <- scale_x_log10(label_long(what), breaks=c(10, 100, 100, 1000, 10000), limits=limits)
    limits <- log10(limits)
  } else {
    limits <- c(as.Date("2014-01-01"), as.Date("2018-01-01"))
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
  rel_widths = c(0.35, 0.68),
  ncol=2
)
dataset_characterisation_plot


cowplot::save_plot(figure_file("dataset_characterisation.svg"), dataset_characterisation_plot, base_height = 6, base_width = 9)



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
      background=technology_colors[technology],
      color="white",
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

