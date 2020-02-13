#' Several figures for looking at the history and diversity of TI methods/tools

#   ____________________________________________________________________________
#   Loading                                                                 ####

library(tidyverse)
library(dynbenchmark)
library(cowplot)

experiment("03-methods/03-method_characterisation")

tools_evaluated <- read_rds(result_file("tools_evaluated.rds", experiment_id = "03-methods"))
tools <- read_rds(result_file("tools.rds", experiment_id = "03-methods"))
tool_qc <- read_rds(result_file("tool_qc.rds", experiment_id = "03-methods"))

tool_id_to_name <- setNames(tools$tool_name, tools$tool_id)

start_date <- as.Date("2014-01-01")
end_date <- as.Date("2019-09-01")


tools$manuscript_publication_date <- tools$manuscript_publication_date


#   ____________________________________________________________________________
#   Figure generation                                                       ####

##  ............................................................................
##  Publication timeline of tools                                         ####

tool_publication_data <- tools %>%
  gather("publication_type", "manuscript_publication_date", c(manuscript_publication_date, manuscript_preprint_date)) %>%
  select(tool_id, publication_type, manuscript_publication_date) %>%
  drop_na() %>%
  mutate(counter = 1)

# add published preprints
tool_publication_data <- tool_publication_data %>%
  group_by(tool_id) %>%
  filter(n() == 2) %>%
  filter(publication_type == "manuscript_publication_date") %>%
  mutate(publication_type = "manuscript_preprint_date", counter = -1) %>%
  bind_rows(tool_publication_data) %>%
  mutate(publication_type = factor(publication_type, levels = c("manuscript_publication_date", "manuscript_preprint_date"))) %>%
  ungroup() %>%
  arrange(manuscript_publication_date)

# summarise by publication type how many tools there are
publication_cumulative_by_type <- tool_publication_data %>%
  group_by(publication_type) %>%
  arrange(manuscript_publication_date) %>%
  mutate(n_tools = cumsum(counter)) %>%
  ungroup()
# add first and last
earliest_date <- start_date
latest_date <- end_date
earliest <- publication_cumulative_by_type %>%
  group_by(publication_type) %>%
  arrange(manuscript_publication_date) %>%
  filter(row_number() == 1) %>%
  mutate(n_tools = 0, manuscript_publication_date = earliest_date, tool_id = NA, counter = 0) %>%
  ungroup()
latest <- publication_cumulative_by_type %>%
  group_by(publication_type) %>%
  arrange(manuscript_publication_date) %>%
  filter(row_number() == n()) %>%
  mutate(manuscript_publication_date = latest_date, tool_id = NA, counter = 0) %>%
  ungroup()
publication_cumulative_by_type <- bind_rows(earliest, publication_cumulative_by_type, latest)

ggplot(publication_cumulative_by_type) +
  geom_step(aes(manuscript_publication_date, n_tools, color = publication_type)) +
  scale_x_date(limits = c(start_date, end_date))

# the following code is very complex, don't try to understand it, I don't either
# for geom_area it is important that every manuscript_date is represented in every group
# therefore we add rows here to add a row for publication when a preprint was published and vice versa
publication_cumulative_by_type_interpolated <- publication_cumulative_by_type %>%
  arrange(manuscript_publication_date) %>%
  complete(publication_type, manuscript_publication_date) %>%
  group_by(publication_type) %>%
  arrange(manuscript_publication_date) %>%
  mutate(counter = ifelse(is.na(counter), 0, counter)) %>%
  mutate(n_tools = cumsum(counter)) %>%
  ungroup()

# now we calculate cumulative number of tools
# although not necessary with geom_area and position = "stack", this is necessary to add labels
publication_cumulative_by_type_interpolated <- publication_cumulative_by_type_interpolated %>%
  group_by(manuscript_publication_date, publication_type) %>%
  slice(n()) %>%
  group_by(manuscript_publication_date) %>%
  arrange(-as.numeric(publication_type)) %>%
  mutate(n_tools_cumulative = cumsum(n_tools)) %>%
  ungroup()

# now we make the plot step-like, by adding an extra row just before every row with n_tools equal to the previous row
publication_cumulative_by_type_interpolated <- publication_cumulative_by_type_interpolated %>%
  group_by(publication_type, manuscript_publication_date) %>%
  slice(n()) %>%
  ungroup() %>%
  arrange(publication_type, manuscript_publication_date) %>%
  mutate(n_tools_cumulative = lag(n_tools_cumulative, 1), manuscript_publication_date = manuscript_publication_date - 1, counter = 0, prefix = TRUE) %>%
  drop_na(n_tools_cumulative) %>%
  bind_rows(publication_cumulative_by_type_interpolated) %>%
  arrange(publication_type, manuscript_publication_date, n_tools_cumulative)

# now we calculate text positions, merge labels when they are on the same row
publication_cumulative_by_type_interpolated_text <- publication_cumulative_by_type_interpolated %>%
  filter(is.na(prefix)) %>%
  group_by(publication_type) %>%
  arrange(manuscript_publication_date) %>%
  mutate(run = rle(n_tools_cumulative) %>% {rep(seq_along(.$lengths), .$lengths)}) %>%
  filter(counter == 1) %>%
  group_by(publication_type, n_tools_cumulative, run) %>%
  summarise(tool_id = paste0(tool_id, collapse = "   "), manuscript_publication_date = min(manuscript_publication_date))

publication_cumulative_text <- tool_publication_data %>%
  group_by(tool_id) %>%
  arrange(manuscript_publication_date) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  mutate(n_tools_cumulative = seq_len(n()))

# now calculate new tools per year
tools_per_year <- tools %>%
  filter(!is.na(manuscript_date)) %>%
  mutate(year = lubridate::year(manuscript_date)) %>%
  count(year) %>%
  mutate(
    min = as.Date(paste0(year, "-01-01")),
    max = lead(min, default = as.Date(end_date)),
    manuscript_date = map2(min, max, ~mean.Date(c(.x, .y))) %>% unlist() %>% as.Date(origin = "1970-01-01")
  ) %>%
  mutate(cumn = cumsum(n))


n_tools_over_time <- publication_cumulative_by_type_interpolated %>%
  mutate(publication_type = c(manuscript_publication_date = "peer_reviewed", manuscript_preprint_date = "preprint")[publication_type]) %>%
  ggplot() +
  geom_area(aes(manuscript_publication_date, n_tools_cumulative, fill = publication_type), position = "identity") +
  scale_fill_manual("", labels = label_long, values = c(peer_reviewed = "#334466", preprint = "#445588")) +
  geom_text(
    aes(
      manuscript_publication_date + 1,
      n_tools_cumulative - 0.5,
      label = tool_id_to_name[tool_id],
      group = publication_type
    ),
    data = publication_cumulative_text,
    color = "white",
    vjust = 0.5,
    hjust = 0,
    fontface = "bold",
    size = 2.5
  ) +
  scale_y_continuous(label_long("n_tools"), expand = c(0, 0), limits = c(0, nrow(tools)+2), breaks = c(0, 20, 40, 60, max(tools_per_year$cumn))) +
  scale_x_date(label_long("publication/manuscript_preprint_date"), expand = c(0.05, 0.05), limits = c(start_date, end_date)) +
  geom_text(aes(manuscript_date, nrow(tools)+1, label = ifelse(year == "2019", stringr::str_glue("{year}: +{n} so far"), stringr::str_glue("{year}: +{n}"))), data = tools_per_year) +
  geom_vline(aes(xintercept = min), data = tools_per_year, linetype = "dashed", color = "grey") +
  theme_pub() +
  theme(legend.position = c(0.05, 0.8))
n_tools_over_time
ggsave(result_file("n_tools_over_time.png"), n_tools_over_time, width = 15, height = 8)
write_rds(n_tools_over_time %>% ggdraw(), result_file("n_tools_over_time.rds"))



##  ............................................................................
##  Platforms                                                               ####
platforms <- tools %>%
  group_by(platform) %>%
  summarise(quantity = n()) %>%
  arrange(quantity) %>%
  mutate(platform = ifelse(is.na(platform), "Not available", platform)) %>%
  mutate(platform = factor(platform, levels = rev(platform))) %>%
  ungroup() %>%
  mutate(pos = cumsum(quantity) - quantity/2) %>%
  ggplot(aes(1, quantity)) +
    geom_bar(aes(fill = platform), width = 1, stat = "identity") +
    geom_text(aes(1, pos, label = stringr::str_glue("{platform} \n {quantity}"), fill = platform), color = "white", fontface = "bold", direction = "y", segment.alpha = 0) +
    theme_void() +
    theme(legend.position = "none") +
    coord_flip() +
    coord_polar("y")
platforms
write_rds(platforms, result_file("platforms.rds"))


##  ............................................................................
##  Trajectory types over time                                              ####
trajectory_type_ids <- dynwrap::trajectory_types %>% filter(id != "convergence") %>% pull(id)

tool_trajectory_types <- tools %>%
  filter(!is.na(manuscript_date)) %>%
  filter(!is.na(trajectory_types)) %>%
  mutate(
    trajectory_types =
      map(trajectory_types, ~as.tibble(set_names(as.list(trajectory_type_ids %in% .), trajectory_type_ids)))
  ) %>%
  unnest(trajectory_types)

tool_trajectory_types <- tool_trajectory_types %>%
  arrange(manuscript_date) %>%
  mutate(count = 1)

add_step <- function(df) {
  df <- df %>%
    group_by(manuscript_date) %>%
    slice(1) %>%
    ungroup() %>%
    mutate(manuscript_date = manuscript_date - 1, prefix = TRUE, count = 0) %>%
    bind_rows(df)
  df %>% arrange(manuscript_date)
}

tool_trajectory_types_step <- add_step(arrange(tool_trajectory_types, manuscript_date))

tool_trajectory_types_gathered <- tool_trajectory_types_step %>%
  gather(trajectory_type, can_trajectory_type, !!trajectory_type_ids) %>%
  mutate(trajectory_type = factor(trajectory_type, levels = simplified_trajectory_types)) %>%
  group_by(trajectory_type) %>%
  arrange(manuscript_date) %>%
  mutate(n_tools = cumsum(count), n_tools_oi = cumsum(count * can_trajectory_type)) %>%
  ungroup() %>%
  group_by(manuscript_date, trajectory_type) %>%
  slice(n()) %>%
  ungroup()

tool_trajectory_types_over_time <- tool_trajectory_types_gathered %>%
  arrange(manuscript_date) %>%
  ggplot() +
  geom_area(aes(manuscript_date, n_tools), stat = "identity", fill = "#BBBBBB") +
  geom_area(aes(manuscript_date, n_tools_oi, fill = trajectory_type), stat = "identity") +
  scale_fill_manual(values = setNames(trajectory_types_simplified$colour, trajectory_types_simplified$simplified)) +
  facet_wrap(~trajectory_type, labeller = label_facet(label_long), nrow = 2) +

  scale_x_date(label_long("manuscript_publication_date"), limits = c(start_date, end_date), breaks = c(start_date, end_date), labels = c("", ""), expand = c(0, 0)) +
  scale_y_continuous(label_long("n_tools"), expand = c(0, 0)) +
  theme_pub() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(legend.position = "none")
tool_trajectory_types_over_time

write_rds(tool_trajectory_types_over_time, result_file("tool_trajectory_types_over_time.rds"))


##  ............................................................................
##  Topology fixation over time                                             ####
topology_inference_timeline_data <- tools_evaluated %>%
  select(manuscript_date, topology_inference) %>%
  mutate(topology_inference = factor(topology_inference, levels = c("fixed", "parameter", "param", "free"))) %>%
  mutate(topology_inference = forcats::fct_collapse(topology_inference, parameter = c("parameter", "param"))) %>%
  mutate(topology_inference_follows = TRUE)
topology_inference_timeline_data <- topology_inference_timeline_data %>%
  bind_rows(
    topology_inference_timeline_data %>% mutate(manuscript_date = manuscript_date-0.2, topology_inference_follows = F),
    tibble(manuscript_date = end_date, topology_inference = factor(levels(topology_inference_timeline_data$topology_inference)), topology_inference_follows = F)
  )

topology_inference_timeline_data <- topology_inference_timeline_data %>%
  complete(topology_inference, manuscript_date, fill = list(topology_inference_follows = FALSE)) %>%
  arrange(manuscript_date, topology_inference) %>%
  ungroup() %>%
  group_by(manuscript_date, topology_inference) %>%
  slice(n()) %>%
  group_by(topology_inference) %>%
  mutate(y = cumsum(topology_inference_follows)) %>%
  ungroup()

topology_inference_timeline <- topology_inference_timeline_data %>% ggplot() +
  geom_area(aes(manuscript_date, y, fill = fct_rev(topology_inference)), stat = "identity") +
  geom_text(aes(max(manuscript_date) - 10, y-(y-lag(y, 1, 0))/2, label = topology_inference), data = topology_inference_timeline_data %>% filter(manuscript_date == end_date) %>% mutate(y = cumsum(y)), hjust = 1, vjust = 0.5, color = "white", size = 5) +
  scale_fill_manual(label_long("topology_inference"), values = setNames(topinf_types$colour, topinf_types$name)) +
  scale_x_date(expand = c(0,0), limits = c(start_date, end_date)) +
  scale_y_continuous(label_long("n_tools"), expand = c(0,0)) +
  theme_pub() +
  theme(legend.position = "none")
topology_inference_timeline
write_rds(topology_inference_timeline, result_file("topology_inference_timeline.rds"))














##  ............................................................................
##  Small tool timeline                                                   ####
tool_small_history_data <- tools %>%
  mutate(
    maximal_trajectory_type = ifelse(is.na(maximal_trajectory_type), "unknown", as.character(maximal_trajectory_type)),
    y = runif(n(), 0, 0.0001)
  ) %>%
  mutate() %>%
  arrange(-y)
tool_small_history <- tool_small_history_data %>%
  ggplot(aes(manuscript_date, y)) +
  ggrepel::geom_label_repel(
    aes(
      fill = maximal_trajectory_type,
      color = maximal_trajectory_type,
      label = tool_id_to_name[tool_id]
    ),
    direction = "y", max.iter = 10000, ylim = c(0, NA), force = 10, min.segment.length = 0) +
  # ggrepel::geom_label_repel(aes(fill = maximal_trajectory_type, label = tool_id, fontface = fontface, size = size), direction = "y", max.iter = 10000, ylim = c(0, NA), force = 10, min.segment.length = 999999) +
  scale_color_manual(values = setNames(trajectory_types$colour, trajectory_types$id)) +
  scale_fill_manual(values = setNames(trajectory_types$colour, trajectory_types$id)) +
  scale_alpha_manual(values = c(`TRUE` = 1, `FALSE` = 0.6)) +
  scale_x_date(label_long("publishing_date"), limits = c(start_date, end_date), date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(NULL, breaks = NULL, limits = c(0, 1), expand = c(0, 0)) +
  scale_size_identity() +
  theme(legend.position = "none")
tool_small_history

ggsave(result_file("tool_small_history.svg"), tool_small_history, height = 4.5, width = 10)

# move lines to back
library(xml2)
svg <- xml2::read_xml(result_file("tool_small_history.svg"))

children <- svg %>% xml_children()
front <- xml_name(children) %in% c("line")
new <- children[!front]
for (node in children[front]) {
  new %>% xml_add_sibling(node, .copy = FALSE, where = c("before"))
}
children[xml_name(children) == "rect"] %>% xml_remove()

# text color
texts <- xml_children(svg) %>% {xml_children(.)} %>% {.[xml_name(.) == "text"]} %>% {.[str_detect(xml_attr(., "style"), "fill:")]}
walk(texts, function(text) {
  # xml_attr(text, "style") <- xml_attr(text, "style") %>% gsub(stringr::str_glue("fill: {toupper(trajectory_types$colour[trajectory_types$id == 'rooted_tree'])};"), "fill: #000;", .)
  xml_attr(text, "style") <- xml_attr(text, "style") %>% gsub("fill: #[A-Z0-9]{6};", "fill: #FFF;", .)
})


xml2::write_xml(svg, result_file("tool_small_history.svg"))



##  ............................................................................
##  Small trajectory type distribution                                      ####

tool_small_distribution <- tools %>%
  gather(trajectory_type, can_handle, !!directed_trajectory_type_order) %>%
  mutate(trajectory_type = factor(trajectory_type, levels = directed_trajectory_type_order)) %>%
  filter(can_handle) %>%
  group_by(trajectory_type) %>%
  count() %>%
  ggplot(aes(trajectory_type, n)) +
  geom_bar(aes(fill = trajectory_type, color = trajectory_type), stat = "identity", width = 0.95) +
  geom_text(aes(label = n), vjust = 0) +
  # geom_hline(yintercept = sum(tools$contains_ti, na.rm = TRUE), line) +
  scale_fill_manual(values = setNames(trajectory_types$background_colour, trajectory_types$id)) +
  scale_color_manual(values = setNames(trajectory_types$colour, trajectory_types$id)) +
  scale_y_continuous(expand = c(0, 2)) +
  theme(legend.position = "None")
tool_small_distribution
ggsave(result_file("tool_small_distribution.svg"), tool_small_distribution, height = 2, width = 5)



tools %>%
  gather(trajectory_type, can_handle, !!directed_trajectory_type_order) %>%
  mutate(trajectory_type = factor(trajectory_type, levels = directed_trajectory_type_order)) %>%
  filter(can_handle) %>%
  group_by(trajectory_type) %>%
  count()

