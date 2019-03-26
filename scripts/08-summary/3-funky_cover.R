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
source(scripts_file("2a_columns_all.R"))

data_sel <- data %>%
  slice(1:10)

# select colums
column_infos_sel <- column_info %>%
  filter(geom %in% c("bar", "funkyrect", "rect") | id == "method_name") %>%
  select(-options, -name)

# create experiments from groups
group_infos_sel <- column_groups %>%
  mutate(experiment = Experiment) %>%
  mutate(color = map_chr(deframe(palettes)[palette], first)) %>%
  select(-Experiment)

experiment_infos_sel <- group_infos_sel %>%
  group_by(experiment) %>%
  summarise(
    palette = first(palette),
    color = first(color)
  )

column_infos_sel <- column_infos_sel %>%
  left_join(group_infos_sel %>% select(experiment, group))

# precalculate column positions, and use those to precalculate positions of groups and experiments
column_infos_sel <- column_infos_sel %>%
  mutate(
    w = case_when(geom == "bar" ~ 4, TRUE ~ 1),
    padding = as.integer(lag(group, default = first(group)) != group),
    x = cumsum(lag(w, default = 0) + padding)
  )

group_infos_sel <- column_infos_sel %>%
  group_by(group) %>%
  summarise(w = max(x + w) - min(x), x = min(x)) %>%
  left_join(group_infos_sel) %>%
  arrange(x)

experiment_infos_sel <- group_infos_sel %>%
  group_by(experiment) %>%
  summarise(w = max(x + w) - min(x), x = min(x)) %>%
  left_join(experiment_infos_sel) %>%
  arrange(x)

# determine row info
row_infos_sel <- tibble(
  id = data_sel$id,
  group = data_sel$method_most_complex_trajectory_type,
  padding = as.integer(lag(group, default = first(group)) != group),
  height = 1
) %>%
  mutate(
    y = cumsum(lag(height, default = 0) + padding),
    y = max(y) - y,

    z = cumsum(padding * 5),
    z = max(z) - z
  )

rowgroup_infos_sel <- row_infos_sel %>%
  group_by(group) %>%
  summarise(
    y = min(y),
    z = min(z),
    height = sum(height)
  ) %>%
  arrange(y)

column_infos_sel %>%
  write_csv(file.path(folder, "column_infos.csv"))

group_infos_sel %>%
  write_csv(file.path(folder, "group_infos.csv"))

experiment_infos_sel %>%
  write_csv(file.path(folder, "experiment_infos.csv"))

row_infos_sel %>%
  write_csv(file.path(folder, "row_infos.csv"))

rowgroup_infos_sel %>%
  write_csv(file.path(folder, "rowgroup_infos.csv"))

# save actual data
data_sel <- data_sel %>%
  select(column_infos_sel$id)

data_sel %>%
  write_csv(file.path(folder, "data.csv"))

