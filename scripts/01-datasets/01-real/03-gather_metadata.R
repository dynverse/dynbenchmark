#' Gathers some metadata about all the real datasets

library(dynbenchmark)
library(googlesheets)
library(tidyverse)

experiment("01-datasets/01-real")


##  ............................................................................
##  Gather metadata                                                         ####
gs_auth()
httr::set_config(httr::config(http_version = 0)) # avoid http2 framing layer bug

# download from google sheets
datasets_real_metadata_source <-
  gs_key("1SALZ2jt7TZJQJMEvvOwSR2r5yIl50qcGAZ-K5AC4DJo") %>%
  gs_read("included", col_types = list(pmid = col_character())) %>%
  tidyr::separate_rows(id, sep = ",\n")

# link the dataset ids to the metadata
dataset_ids_real <- list_datasets("real") %>% pull(id)

datasets_real_metadata <-
  datasets_real_metadata_source %>%
  mutate(id = map(paste0("real/(gold|silver)/", datasets_real_metadata_source$id), str_match, string = dataset_ids_real) %>% map(~.[,1]) %>% map(~.[!is.na(.)])) %>%
  unnest(id)

if (any(!dataset_ids_real %in% datasets_real_metadata$id)) {
  stop("No metadata found for : ", setdiff(dataset_ids_real, datasets_real_metadata$id) %>% glue::glue_collapse(", "))
}

# add some information on the trajectory itself
datasets_real <- load_datasets(dataset_ids_real)

datasets_real_metadata <- datasets_real_metadata %>%
  left_join(
    datasets_real %>%
      mutate(
        n_cells = map_int(cell_ids, length),
        n_features = map_int(feature_info, nrow)
      ) %>%
      select(id, trajectory_type, n_cells, n_features),
    "id"
  )

write_rds(datasets_real_metadata, result_file("metadata.rds"))


##  ............................................................................
##  Characterise                                                            ####

# Define colours
technology_colours <- unique(datasets_real_metadata$technology) %>%
  setNames(RColorBrewer::brewer.pal(length(.), "Set3"), .)
standard_colours <- c("gold" = "#ffeca9", "silver" = "#e3e3e3")


#   ____________________________________________________________________________
#   Overview figure of different datasets                                   ####
plot_real_characteristics <- cowplot::plot_grid(
  nrow = 1,
  plotlist = map(
    c("technology", "organism", "standard","dynamic_process", "trajectory_type"),
    function(what) {
      datasets <- datasets_real_metadata

      if (!what %in% names(datasets)) {stop(what)}
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
        geom_text(aes(mid, text_position, label = stringr::str_glue(label), vjust = 0.5), color = "black", hjust = 0.5, size = 4) +
        fill_scale +
        ggtitle(label_long(what)) +
        theme_void() +
        theme(legend.position = "none") +
        scale_x_continuous(expand = c(0, 0)) +
        scale_y_continuous(expand = c(0, 0)) +
        coord_flip()
    }
  ))
write_rds(plot_real_characteristics, result_file("characteristics.rds"))



# Create dotplot
plot_sizes <- cowplot::plot_grid(
  ncol = 1,
  align = "v",
  axis = "lr",
  plotlist = map(
    c("n_cells", "n_features", "date"),
    function(what) {
      nbins <- 70

      if (what %in% c("n_cells", "n_features")) {
        limits <- c(10, 30000)
        x_scale <- scale_x_log10(label_long(what), breaks = c(10, 100, 100, 1000, 10000), limits = limits)
        limits <- log10(limits)
      } else {
        limits <- c(as.Date("2014-01-01"), as.Date("2019-01-01"))
        x_scale <- scale_x_date(label_long(what), limits = limits, date_breaks = "1 year", date_labels = "%Y")
      }

      ggplot(datasets_real_metadata, aes_string(what, fill = "technology", color = "technology")) +
        geom_dotplot(
          binwidth = as.numeric(max(limits) - min(limits)) / nbins,
          method = "histodot",
          binpositions = "all",
          stackgroups = TRUE,
          stackratio = 1.5,
          dotsize = 0.4
        ) +
        x_scale +
        scale_y_continuous(NULL, breaks = NULL, expand = c(0.02, 0)) +
        scale_fill_manual(values = technology_colours) +
        scale_color_manual(values = technology_colours) +
        theme(legend.position = "none") +
        theme_pub()
    })
)

write_rds(plot_sizes, result_file("sizes.rds"))
