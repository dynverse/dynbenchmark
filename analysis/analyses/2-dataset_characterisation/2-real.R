library(tidyverse)
library(cowplot)
library(googlesheets)
library(dynalysis)

experiment("2-dataset_characterisation/2-real")

# Fetch tasks
tasks_real <- load_datasets_tibble() %>% filter(category == "real")

# Define colours
technology_colours <- unique(tasks_real$technology) %>%
  setNames(RColorBrewer::brewer.pal(length(.), "Set1"), .)
standard_colours <- c("gold" = "#ffeca9", "silver" = "#e3e3e3")

#######################################
## Dataset characteristics bar chart ##
#######################################
dataset_characterisation_boxes <- cowplot::plot_grid(
  nrow = 1,
  plotlist = map(
    c("technology", "organism", "standard","dynamic_process", "trajectory_type"),
    function(what) {
      tasks <- tasks_real
      tasks$variable_of_interest <- tasks[[what]]

      # extract the variable and calculate the positions
      pie_data <- tasks %>%
        count(variable_of_interest) %>%
        arrange(n) %>%
        mutate(
          odd = row_number() %% 2,
          row_number = row_number(),
          start = lag(cumsum(n), 1, 0),
          end = cumsum(n),
          mid = start + (end - start) / 2
        )

      # change labels, fill or order depending on variable
      label <- "{label_short(variable_of_interest, 15)}: {n}"
      fill_scale <- scale_fill_grey(start = 0.6, end = 0.9)
      text_position <- 0.5

      if (what == "standard") {
        fill_scale <- scale_fill_manual(values=standard_colours)
      } else if (what == "technology") {
        fill_scale <- scale_fill_manual(values=technology_colours)
      } else if (what == "trajectory_type") {
        fill_scale <- scale_fill_manual(values=setNames(trajectory_types$background_colour,trajectory_types$id))
      }

      ggplot(pie_data, aes(ymin=0, ymax=1)) +
        geom_rect(aes(xmin=start, xmax=end, fill=variable_of_interest), stat="identity", color="white", size=0.5) +
        geom_text(aes(mid, text_position, label = pritt(label), vjust=0.5), color="black", hjust=0.5, size=4) +
        fill_scale +
        ggtitle(label_long(what)) +
        theme_void() +
        theme(legend.position = "none") +
        scale_x_continuous(expand=c(0, 0)) +
        scale_y_continuous(expand=c(0, 0)) +
        coord_flip()
    }
  ))
dataset_characterisation_boxes

ggsave(figure_file("dataset_characterisation_boxes.svg"), dataset_characterisation_boxes, width = 10, height = 8)



##################################################################################################
## TODO robrecht; WIP                                                                           ##
##################################################################################################


##  ............................................................................
##  Distributions                                                           ####
dataset_characterisation_distributions <- c("ncells", "ngenes", "date") %>% map(function(what) {
  nbins <- 70

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
    scale_fill_manual(values=technology_colours) +
    scale_color_manual(values=technology_colours) +
    theme(legend.position="none")
})
# dataset_characterisation_distributions[[2]] <- dataset_characterisation_distributions[[2]] + theme(legend.position="right")
dataset_characterisation_distributions <- cowplot::plot_grid(plotlist=dataset_characterisation_distributions, ncol=1, align="v", axis="lr")


dataset_characterisation_distributions
dataset_characterisation_distributions %>% write_rds(figure_file("dataset_characterisation_distributions.rds"))


##  ............................................................................
##  Small characterisation figure                                           ####
dataset_characterisation_plot <- cowplot::plot_grid(
  dataset_characterisation_distributions,
  dataset_characterisation_pies,
  rel_heights = c(1,1.5),
  ncol=1
)
dataset_characterisation_plot

cowplot::save_plot(figure_file("dataset_characterisation.svg"), dataset_characterisation_plot, base_height = 10, base_width = 10)



##  ............................................................................
##  Datasets table                                                          ####
grouper <- list(
  latex=function(x) paste0("\\iffalse {label_long(gse)} \\fi {", x, "}"),
  html=function(x) paste0("<span style='display: none;'>{gse}</span> {", x, "}")
)

library(kableExtra)

table <- map(c("latex", "html"), function(format) {
  table <- tasks_real %>%
    select(date, gse, organism, technology, id, trajectory_type, standard, standard_determination, ncells, ngenes) %>%
    arrange(date) %>%
    select(-date) %>%
    mutate(
      # date = cell_spec(
      #   glue::glue(grouper[[format]]("date")),
      #   format,
      #   escape=FALSE
      # ),
      technology = cell_spec(
        glue::glue(grouper[[format]]("technology")),
        format,
        background=technology_colours[technology],
        color="white",
        escape=FALSE
      ),
      organism = cell_spec(
        glue::glue(grouper[[format]]("organism")),
        format,
        escape=FALSE
      ),
      trajectory_type = kableExtra::cell_spec(
        label_long(trajectory_type),
        format,
        background=toupper(set_names(trajectory_types$colour, trajectory_types$id)[trajectory_type]),
        color=ifelse(trajectory_type == "rooted_tree", "#333333", "#FFFFFF"),
      ),
      ncells = cell_spec(
        ncells,
        format,
        background=spec_color(log(ncells), end = 0.9, option = "D", direction = 1),
        escape=FALSE,
        color="white"
      ),
      ngenes = cell_spec(
        ngenes,
        format,
        background=spec_color(log(ngenes), end = 0.9, option = "D", direction = 1),
        escape=FALSE,
        color="white"
      ),
      standard_determination = cell_spec(
        standard_determination,
        format,
        background=toupper(standard_colours[standard])
      )
    ) %>%
    select(-id, -standard) %>%
    rename_all(label_short, width=99999999) %>%
    rename_all(function(x) if(format == "latex") {gsub("\\#", "\\\\#", x)} else {x}) %>%
    knitr::kable(format, escape=FALSE) %>%
    collapse_rows(1:2) %>%
    kable_styling(bootstrap_options = c("hover", "striped"), latex_options = c("striped", "scale_down"), font_size=7)
  table
}) %>% set_names(c("latex", "html"))
table

table %>%
  write_rds(figure_file("datasets.rds"))


##  ............................................................................
##  Plots of each dataset                                                   ####
library(tidygraph)
library(ggraph)

label_dataset <- function(x) {
  gsub(".*/(.*)", "\\1", x) %>% gsub("-", " ", .) %>% gsub("_(.*)", ": \\U\\1 et al.", .) %>% label_capitalise()
}

plots <- seq_len(nrow(tasks_real)) %>% map(function(task_i) {
  task <- dynutils::extract_row_to_list(tasks_real, task_i)

  milestone_graph <- tbl_graph(tibble(node=factor(task$milestone_ids, levels=task$milestone_ids)), task$milestone_network %>% mutate(from = match(from, task$milestone_ids), to = match(to, task$milestone_ids)))

  # determine layout
  layout <- "tree"
  if (task$trajectory_type == "directed_cycle") {
    layout <- "circle"
  }

  milestone_graph %>%
    ggraph(layout=layout) +
      geom_edge_link() +
      geom_edge_link(aes(xend=x + (xend - x)/1.6, yend = y + (yend - y)/1.6), arrow=arrow(type="closed")) +
      geom_node_label(aes(label=node, fill=node)) +
      theme_graph() +
      theme(legend.position="none", plot.margin = margin(1, 1, 1, 1)) +
      scale_x_continuous(expand = c(1,1)) +
      ggtitle(label_dataset(task$id))
})
cowplot::plot_grid(plotlist=plots[1:10], ncol=5)





##  ............................................................................
##  Small pies                                                              ####
##  ............................................................................
##  Pie charts                                                              ####
pie <- function(tasks=tasks_real, what = "technology") {
  tasks$variable_of_interest <- tasks[[what]]

  # first extract the variable
  pie_data <- tasks %>%
    count(variable_of_interest) %>%
    arrange(n)

  label_legend <- function(x) {
    map_chr(x, ~pritt("{label_short(., Inf)}: {sum(pie_data$n[pie_data$variable_of_interest == .])}"))
  }

  # change labels, fill or order depending on variable
  label <-"{label_short(variable_of_interest, 15)}: {n}"
  fill_scale <- scale_fill_grey(start = 0.6, end = 0.9)
  text_position <- 0.5
  if (what == "technology") {
    fill_scale <- scale_fill_manual(
      "",
      values = technology_colours[pie_data$variable_of_interest],
      labels = label_legend(pie_data$variable_of_interest),
      breaks = pie_data$variable_of_interest
    )
  } else if (what == "trajectory_type") {
    pie_data <- pie_data %>% arrange(
      -match(as.character(variable_of_interest), trajectory_types$id)
    )
    fill_scale <- scale_fill_manual(
      "",
      values = setNames(trajectory_types$colour,trajectory_types$id)[pie_data$variable_of_interest],
      labels = label_legend(pie_data$variable_of_interest),
      breaks = pie_data$variable_of_interest
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
    theme_void() +
    scale_x_continuous(expand=c(0, 0)) +
    scale_y_continuous(expand=c(0, 0)) +
    fill_scale +
    coord_polar()

  pie
}

dataset_characterisation_pies <- c("technology", "trajectory_type") %>% map(pie, tasks = tasks_real) %>% cowplot::plot_grid(plotlist=., nrow =1)
dataset_characterisation_pies
dataset_characterisation_pies %>% write_rds(figure_file("dataset_characterisation_pies.rds"))

dataset_characterisation_pies %>% ggsave(figure_file("dataset_characterisation_pies_small.svg"), ., width=6, height=3)


