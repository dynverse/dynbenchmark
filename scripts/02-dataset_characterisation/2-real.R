library(tidyverse)
library(cowplot)
library(dynbenchmark)

experiment("02-dataset_characterisation/2-real")

datasets_real <- load_datasets(list_datasets() %>% filter(dataset_source == "real") %>% pull(dataset_id), as_tibble = TRUE)


#   ____________________________________________________________________________
#   Add dimensionality reduction                                            ####
datasets_real <- mapdf(datasets_real, add_dimred, dimred = dyndimred::dimred_umap) %>% list_as_tibble()

datasets_real_plots <- tibble(
  dataset_id = datasets_real$id,
  topology = mapdf(datasets_real, plot_topology)
)






plots <- mapdf(datasets_real, plot_dimred, color_cells = "grouping")


plot_dimred(extract_row_to_list(datasets_real, 59))



datasets_real



dataset <- load_dataset("real/aging-hsc-old_kowalczyk")
















# Define colours
technology_colours <- unique(datasets_real$technology) %>%
  setNames(RColorBrewer::brewer.pal(length(.), "Set1"), .)
standard_colours <- c("gold" = "#ffeca9", "silver" = "#e3e3e3")


#   ____________________________________________________________________________
#   Overview figure of different datasets                                   ####
boxes <- cowplot::plot_grid(
  nrow = 1,
  plotlist = map(
    c("technology", "organism", "standard","dynamic_process", "trajectory_type"),
    function(what) {
      datasets <- datasets_real
      datasets$variable_of_interest <- datasets[[what]]

      # extract the variable and calculate the positions
      pie_data <- datasets %>%
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
        fill_scale <- scale_fill_manual(values = standard_colours)
      } else if (what == "technology") {
        fill_scale <- scale_fill_manual(values = technology_colours)
      } else if (what == "trajectory_type") {
        fill_scale <- scale_fill_manual(values = setNames(trajectory_types$background_colour,trajectory_types$id))
      }

      ggplot(pie_data, aes(ymin = 0, ymax = 1)) +
        geom_rect(aes(xmin = start, xmax = end, fill = variable_of_interest), stat = "identity", color = "white", size = 0.5) +
        geom_text(aes(mid, text_position, label = pritt(label), vjust = 0.5), color = "black", hjust = 0.5, size = 4) +
        fill_scale +
        ggtitle(label_long(what)) +
        theme_void() +
        theme(legend.position = "none") +
        scale_x_continuous(expand = c(0, 0)) +
        scale_y_continuous(expand = c(0, 0)) +
        coord_flip()
    }
  ))
boxes

ggsave(figure_file("dataset_characterisation_boxes.svg"), boxes, width = 10, height = 5.5)


# Generate small pie plots as an alternative for the previous box plots
pies <- cowplot::plot_grid(
  nrow = 1,
  plotlist = map(
    c("technology", "trajectory_type"),
    function(what) {
      datasets <- datasets_real
      datasets$variable_of_interest <- datasets[[what]]

      # first extract the variable
      pie_data <- datasets %>%
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

      # now calculate positions
      pie_data <- pie_data %>% mutate(odd = row_number()%%2, row_number = row_number()) %>%
        mutate(start = lag(cumsum(n), 1, 0), end = cumsum(n), mid = start + (end - start)/2)

      pie_data %>%
        ggplot(aes(ymin = 0, ymax = 1)) +
        geom_rect(aes(xmin = start, xmax = end, fill = variable_of_interest), stat = "identity", color = "white", size = 0.5) +
        theme_void() +
        scale_x_continuous(expand = c(0, 0)) +
        scale_y_continuous(expand = c(0, 0)) +
        fill_scale +
        coord_polar()
    }
  )
)

ggsave(figure_file("dataset_characterisation_pies_small.svg"), pies, width = 6, height = 3)




# Create dotplot
dotplots <- cowplot::plot_grid(
  ncol = 1,
  align = "v",
  axis = "lr",
  plotlist = map(
    c("n_cells", "n_genes", "date"),
    function(what) {
      nbins <- 70

      if (what %in% c("n_cells", "n_genes")) {
        limits <- c(10, 30000)
        x_scale <- scale_x_log10(label_long(what), breaks = c(10, 100, 100, 1000, 10000), limits = limits)
        limits <- log10(limits)
      } else {
        limits <- c(as.Date("2014-01-01"), as.Date("2018-06-01"))
        x_scale <- scale_x_date(label_long(what), limits = limits, date_breaks = "1 year", date_labels = "%Y")
      }

      ggplot(datasets_real, aes_string(what, fill = "technology", color = "technology")) +
        geom_dotplot(
          binwidth = as.numeric(max(limits) - min(limits)) / nbins,
          method = "histodot",
          binpositions = "all",
          stackgroups = TRUE,
          stackratio = 1.5,
          dotsize = 0.7
        ) +
        x_scale +
        scale_y_continuous(NULL, breaks = NULL, expand = c(0.02, 0)) +
        scale_fill_manual(values = technology_colours) +
        scale_color_manual(values = technology_colours) +
        theme(legend.position = "none")
    })
)

dotplots

ggsave(figure_file("dataset_characterisation_distributions.svg"), dotplots, width = 10, height = 4.5)


# Combined plot
combined <- cowplot::plot_grid(
  dotplots,
  boxes,
  rel_heights = c(1, 1.5),
  ncol = 1
)
combined
ggsave(figure_file("dataset_characterisation.svg"), combined, width = 10, height = 10)




#   ____________________________________________________________________________
#   Overview table of different datasets                                    ####
grouper <- list(
  latex = function(x) paste0("\\iffalse {label_long(gse)} \\fi {", x, "}"),
  html = function(x) paste0("<span style = 'display: none;'>{gse}</span> {", x, "}")
)

library(kableExtra)

table <- map(c("latex", "html"), function(format) {
  table <- datasets_real %>%
    select(date, gse, organism, technology, id, trajectory_type, standard, standard_determination, n_cells, n_genes) %>%
    arrange(date) %>%
    select(-date) %>%
    mutate(
      technology = cell_spec(
        glue::glue(grouper[[format]]("technology")),
        format,
        background = technology_colours[technology],
        color = "white",
        escape = FALSE
      ),
      organism = cell_spec(
        glue::glue(grouper[[format]]("organism")),
        format,
        escape = FALSE
      ),
      trajectory_type = kableExtra::cell_spec(
        label_long(trajectory_type),
        format,
        background = toupper(set_names(trajectory_types$colour, trajectory_types$id)[trajectory_type]),
        color = ifelse(trajectory_type == "rooted_tree", "#333333", "#FFFFFF")
      ),
      n_cells = cell_spec(
        n_cells,
        format,
        background = spec_color(log(n_cells), end = 0.9, option = "D", direction = 1),
        escape = FALSE,
        color = "white"
      ),
      n_genes = cell_spec(
        n_genes,
        format,
        background = spec_color(log(n_genes), end = 0.9, option = "D", direction = 1),
        escape = FALSE,
        color = "white"
      ),
      standard_determination = cell_spec(
        standard_determination,
        format,
        background = toupper(standard_colours[standard])
      )
    ) %>%
    select(-id, -standard) %>%
    rename_all(label_short, width = 99999999) %>%
    rename_all(function(x) if(format == "latex") {gsub("\\#", "\\\\#", x)} else {x}) %>%
    knitr::kable(format, escape = FALSE) %>%
    collapse_rows(1:2) %>%
    kable_styling(bootstrap_options = c("hover", "striped"), latex_options = c("striped", "scale_down"), font_size = 7)
  table
}) %>% set_names(c("latex", "html"))
table

write_rds(table, figure_file("datasets.rds"))



##  ............................................................................
##  Milestone network of every dataset                                      ####
label_dataset <- function(x) {
  x %>%
    str_replace_all(".*/(.*)", "\\1") %>%
    str_replace_all("-", " ") %>%
    str_replace("_(.*)", ": \\1 et al.") %>%
    label_capitalise()
}

plots <- seq_len(nrow(datasets_real)) %>% map(function(dataset_i) {
  dataset <- dynutils::extract_row_to_list(datasets_real, dataset_i)

  plot_topology(dataset)
})
cowplot::plot_grid(plotlist = plots[1:10], ncol = 5)



