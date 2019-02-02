library(tidyverse)
library(dynbenchmark)

experiment("08-summary")

folder <- "../../dyndocs/funky_cover/data/"

# save all palettes
palettes <- tribble(
  ~palette,        ~colours,
  # blues palette
  "overall", grDevices::colorRampPalette(rev(RColorBrewer::brewer.pal(9, "Greys")[-1]))(101),
  "benchmark", grDevices::colorRampPalette(rev(RColorBrewer::brewer.pal(9, "Blues") %>% c("#011636")))(101),
  "scaling", grDevices::colorRampPalette(rev(RColorBrewer::brewer.pal(9, "Reds")[-8:-9]))(101),
  "stability", grDevices::colorRampPalette(rev(RColorBrewer::brewer.pal(9, "YlOrBr")[-7:-9]))(101),
  "qc", grDevices::colorRampPalette(rev(RColorBrewer::brewer.pal(9, "Greens")[-1] %>% c("#00250f")))(101),
  "white6black4", c(rep("white", 3), rep("black", 7)) %>% gplots::col2hex(),
  "column_annotation", c(overall = "#555555", benchmark = "#4292c6", scaling = "#f6483a", stability = "#fe9929", qc = "#41ab5d")
)

jsonlite::write_json(palettes %>% deframe(), file.path(folder, "palettes.json"))


# save columns and groups
source(scripts_file("2-main_figure.R"))

column_info_sel <- column_info %>%
  filter(geom %in% c("bar", "funkyrect", "rect")) %>%
  select(-options)
column_info_sel %>%
  write_csv(file.path(folder, "column_info.csv"))

column_groups %>%
  write_csv(file.path(folder, "column_groups.csv"))

# save actual data
data_sel <- data %>%
  select(column_info_sel$id)

data_sel %>%
  write_csv(file.path(folder, "data.csv"))
