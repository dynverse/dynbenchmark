library(tidyverse)
library(dynalysis)
library(cowplot)

experiment("method_characteristics")

source("analysis/analyses/4-method_characterization/0_common.R")

method_df <- read_rds(derived_file("method_df.rds"))
method_df_evaluated <- read_rds(derived_file("method_df_evaluated.rds"))

# Comparing number of citations, qc score, and time -------------------------------------
g1 <- ggplot(method_df_evaluated, aes(date, qc_score)) +
  geom_smooth(span=2) +
  geom_point() +
  ggrepel::geom_label_repel(aes(label = name)) +
  cowplot::theme_cowplot() +
  scale_y_continuous(breaks = c(0, 2, 4, 6, 8), limits = c(0, 8)) +
  labs(x = "Time", y = "QC score", title = "Code quality score over time")
g1

write_rds(g1, figure_file("ncitations_over_time.rds"))
ggsave(figure_file("time_vs_qcscore.png"), g1, width = 16, height = 8)

g2 <- ggplot(method_df_evaluated, aes(date, Citations+1)) +
  geom_smooth(method="lm") +
  geom_point() +
  ggrepel::geom_label_repel(aes(label = name), nudge_y = .25) +
  cowplot::theme_cowplot() +
  scale_y_log10() +
  labs(x = "Time", y = "Citations", title = "Citations over time")
g2
ggsave(figure_file("time_vs_citations.png"), g2, width = 16, height = 8)

g3 <- ggplot(method_df_evaluated, aes(Citations+1, qc_score)) +
  geom_smooth(method="lm") +
  geom_point() +
  ggrepel::geom_label_repel(aes(label = name), nudge_y = .25) +
  cowplot::theme_cowplot() +
  scale_x_log10() +
  labs(x = "Citations", y = "QC score", title = "QC score over # citations")
g3
ggsave(figure_file("citations_vs_qcscore.png"), g3, width = 8, height = 8)


# Publication timeline of methods -----------------
method_publication_data <- method_df %>% filter(is_ti == "Yup") %>%
  gather("publication_type", "publication_date", c(PubDate, Preprint)) %>%
  select(name, publication_type, publication_date) %>%
  drop_na() %>%
  mutate(counter = 1)

# add published preprints
method_publication_data <- method_publication_data %>%
  group_by(name) %>%
  filter(n() == 2) %>%
  filter(publication_type == "PubDate") %>%
  mutate(publication_type = "Preprint", counter = -1) %>%
  bind_rows(method_publication_data) %>%
  mutate(publication_type = factor(publication_type, levels=c("PubDate", "Preprint")))

# summarise by publication type how many methods there are
publication_cumulative_by_type <- method_publication_data %>%
  group_by(publication_type) %>%
  arrange(publication_date) %>%
  mutate(n_methods = cumsum(counter)) %>%
  ungroup()
# add first and last
earliest_date <- min(publication_cumulative_by_type$publication_date)
latest_date <- Sys.Date() + 100
earliest <- publication_cumulative_by_type %>%
  group_by(publication_type) %>%
  arrange(publication_date) %>%
  filter(row_number() == 1) %>%
  mutate(n_methods = 0, publication_date = earliest_date, name = NA, counter=0) %>%
  ungroup()
latest <- publication_cumulative_by_type %>%
  group_by(publication_type) %>%
  arrange(publication_date) %>%
  filter(row_number() == n()) %>%
  mutate(publication_date = latest_date, name = NA, counter = 0) %>%
  ungroup()
publication_cumulative_by_type <- bind_rows(earliest, publication_cumulative_by_type, latest)

ggplot(publication_cumulative_by_type) +
  geom_step(aes(publication_date, n_methods, color=publication_type))

# the following code is very complex, don't try to understand it, I don't either
# for geom_area it is important that every date is represented in every group
# therefore we add rows here to add a row for publication when a preprint was published and vice versa
publication_cumulative_by_type_interpolated <- publication_cumulative_by_type %>%
  arrange(publication_date) %>%
  complete(publication_type, publication_date) %>%
  group_by(publication_type) %>%
  arrange(publication_date) %>%
  mutate(counter = ifelse(is.na(counter), 0, counter)) %>%
  mutate(n_methods = cumsum(counter)) %>%
  ungroup()

# now we calculate cumulative number of methods
# although not necessary with geom_area and position="stack", this is necessary to add labels
publication_cumulative_by_type_interpolated <- publication_cumulative_by_type_interpolated %>%
  group_by(publication_date) %>%
  arrange(-as.numeric(publication_type)) %>%
  mutate(n_methods_cumulative = cumsum(n_methods)) %>%
  ungroup()

# now we make the plot step-like, by adding an extra row just before every row with n_methods equal to the previous row
publication_cumulative_by_type_interpolated <- publication_cumulative_by_type_interpolated %>%
  arrange(publication_type, publication_date) %>%
  mutate(n_methods_cumulative = lag(n_methods_cumulative, 1), publication_date = publication_date - 0.1, counter=0, prefix=TRUE) %>%
  drop_na(n_methods_cumulative) %>%
  bind_rows(publication_cumulative_by_type_interpolated) %>%
  arrange(publication_type, publication_date, n_methods_cumulative)

# now we calculate text positions, merge labels when they are on the same row
publication_cumulative_by_type_interpolated_text <- publication_cumulative_by_type_interpolated %>%
  filter(is.na(prefix)) %>%
  group_by(publication_type) %>%
  arrange(publication_date) %>%
  mutate(run = rle(n_methods_cumulative) %>% {rep(seq_along(.$lengths), .$lengths)}) %>%
  filter(counter == 1) %>%
  group_by(publication_type, n_methods_cumulative, run) %>%
  summarise(name = paste0(name, collapse="   "), publication_date = min(publication_date))

publication_cumulative_text <- method_publication_data %>%
  group_by(name) %>%
  arrange(publication_date) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  mutate(n_methods_cumulative = seq_len(n()))


n_methods_over_time <- publication_cumulative_by_type_interpolated %>%
  ggplot() +
    geom_area(aes(publication_date, n_methods_cumulative, fill=publication_type), position="identity") +
    geom_text(
      aes(publication_date + 1, n_methods_cumulative - 0.5, label=name, group=publication_type),
      data = publication_cumulative_text,
      color="white",
      vjust=0.5,
      hjust=0,
      fontface = "bold",
      size=3
    ) +
    theme(legend.position = c(0.05, 0.95), legend.background = element_rect(color = "black", fill = "white", size = 0.5, linetype = "solid"))
n_methods_over_time
# ggsave(figure_file("n_methods_over_time.png"), n_methods_over_time, width = 15, height = 8)
saveRDS(n_methods_over_time, figure_file("n_methods_over_time.rds"))



# Statistics -------------------------------------
# Platforms ------------------------------
platforms <- method_df_evaluated %>% separate_rows(platform=platforms, sep=", ") %>%
  group_by(platforms) %>%
  summarise(quantity = n()) %>%
  arrange(quantity) %>%
  mutate(platform = ifelse(is.na(platforms), "Not available", platforms)) %>%
  mutate(platform = factor(platform, levels=rev(platform))) %>%
  ungroup() %>%
  mutate(pos = cumsum(quantity) - quantity/2) %>%
  ggplot(aes(1, quantity)) +
    geom_bar(aes(fill=platform), width = 1, stat="identity") +
    geom_text(aes(1, pos, label=pritt("{platform} \n {quantity}"), fill=platform), color="white", fontface = "bold", direction = "y", segment.alpha=0) +
    theme_void() +
    theme(legend.position="none") +
    coord_flip() +
    coord_polar("y")
platforms
saveRDS(platforms, figure_file("platforms.rds"))


# Trajectory components over time -------------------------------------------
trajectory_components <- method_df_evaluated
trajectory_components <- trajectory_components %>%
  arrange(date) %>%
  mutate(count = 1)

add_step <- function(df) {
  df <- df %>%
    mutate(date = date - 0.2, prefix=TRUE, count = 0) %>%
    bind_rows(df)
  df %>% arrange(date)
}

trajectory_components_step <- add_step(arrange(trajectory_components, date))

trajectory_components_gathered <- trajectory_components_step %>%
  gather(trajectory_type, can_trajectory_type, !!trajectory_types) %>%
  mutate(trajectory_type = factor(trajectory_type, levels=trajectory_types)) %>%
  group_by(trajectory_type) %>%
  arrange(date) %>%
  mutate(n_methods = cumsum(count), n_methods_oi = cumsum(count * can_trajectory_type))

trajectory_components_over_time <- trajectory_components_gathered %>%
  arrange(date) %>%
  ggplot() +
  geom_area(aes(date, n_methods), stat="identity") +
  geom_area(aes(date, n_methods_oi, fill=trajectory_type), stat="identity") +
  scale_fill_manual(values=trajectory_type_colors) +
  facet_grid(.~trajectory_type) +
  theme(legend.position = "none")

trajectory_components_over_time

saveRDS(trajectory_components_over_time, figure_file("trajectory_components_over_time.rds"))


# Method aspects -------------------------------------------
method_order <- method_df_evaluated %>% arrange(qc_score) %>% pull(name)
horizontal_lines <- geom_hline(aes(yintercept = y+0.5), alpha=0.2, data=tibble(y=seq_along(method_order)))

rotated_axis_text_x <- element_text(angle=45, hjust=1)

base_theme <- theme(
  plot.margin = unit(c(2, 0, 0.5, 0), "lines"),
  axis.text.x=rotated_axis_text_x,
  legend.position = "top",
  legend.box = "horizontal"
)
empty_left_theme <- theme(
  axis.text.y = element_blank(),
  axis.title.y = element_blank(),
  axis.line.y = element_blank(),
  axis.ticks.y = element_blank()
)
base_scale_y <- scale_y_discrete(drop=FALSE, expand=c(0, 0))
base_scale_x <- scale_x_discrete(drop=FALSE, expand=c(0, 0))



# QC
qcs <- c("code_availability", "documentation", "examples", "unit_tests", "behaviour", "code_quality", "support")

traffic_light <- list(low = "#FF4136", mid = "#FF851B", high = "#2ECC40")

score_max <- set_names(rep(1, length(qcs)), qcs)
score_max[c("behaviour", "code_quality")] <- 0.5
method_qc_individual <- method_df_evaluated %>%
  select(name, !!qcs) %>%
  gather(score_id, score, !!qcs, factor_key=TRUE) %>%
  mutate(
    score_text = ifelse(score == 0, "-", ifelse(score == score_max[score_id], "+", "~")),
    score_fill = ifelse(score == 0, traffic_light$low, ifelse(score == score_max[score_id], traffic_light$high, traffic_light$mid)),
    name = factor(name, levels=method_order)
  ) %>%
  ggplot(aes(score_id, name)) +
    geom_tile(aes(fill=score_fill)) +
    scale_fill_identity() +
    geom_text(aes(label = score_text), color="white", size=8, fontface = "bold") +
    theme(axis.text.x=rotated_axis_text_x) +
    base_theme +
    empty_left_theme +
    horizontal_lines +
    base_scale_y + base_scale_x +
    ggtitle("Implementation quality control")
method_qc_individual
method_qc_individual_width <- length(qcs)

method_qc_overall <- method_df_evaluated %>%
  select(name, qc_score) %>%
  mutate(name = factor(name, levels=method_order)) %>%
  ggplot(aes("qc_score", name)) +
    geom_tile(aes(fill = qc_score)) +
    scale_fill_gradientn(colours=traffic_light) +
    #viridis::scale_fill_viridis(option="A", guide=guide_colorbar(direction = "horizontal")) +
    base_theme +
    empty_left_theme +
    horizontal_lines +
    base_scale_y + base_scale_x +
    theme(legend.position = "none")
method_qc_overall
method_qc_overall_width <- 1

# Prior information
priors <- c("start_cell_ids", "grouping_assignment", "time", "timecourse",	"end_cell_ids",	"n_end_states","n_branches", "marker_gene_ids")
prior_usage_colors <- c("?" = "#AAAAAA", "No" = "#EEEEEE", "Required" = "#FF4136", "CanUse" = "#0074D9", "CanRoot" = "#39CCCC", "CanUseSingle" = "#c5e4ff")
prior_information <- method_df_evaluated %>%
  gather(prior_id, prior_usage, !!priors, factor_key=TRUE) %>%
  select(prior_id, prior_usage, name) %>%
  mutate(name = factor(name, levels=method_order)) %>%
  drop_na() %>%
  ggplot() +
    geom_tile(aes(prior_id, name, fill = prior_usage)) +
  geom_text(aes(prior_id, name, label=ifelse(prior_usage == "No", "-", prior_usage))) +
    scale_fill_manual(values=prior_usage_colors) +
    base_theme +
    empty_left_theme +
    horizontal_lines +
    base_scale_y + base_scale_x +
    theme(legend.position = "none") +
    ggtitle("Prior information")
prior_information


prior_usages <- c("CanRoot", "CanUse", "Required")
prior_usages_text <- c(CanRoot = "", CanUse = "", Required = "!", No = "")
prior_usage <- method_df_evaluated %>%
  gather(prior_id, prior_usage, !!priors, factor_key=TRUE) %>%
  select(prior_id, prior_usage, name) %>%
  group_by(name, prior_usage) %>%
  summarise() %>%
  ungroup() %>%
  mutate(
    prior_usage = factor(prior_usage, levels=prior_usages),
    name = factor(name, levels=method_order)
  ) %>%
  drop_na() %>%
  mutate(really = TRUE) %>%
  complete(name, prior_usage) %>%
  mutate(really = ifelse(is.na(really), "no", "yes")) %>%
  ggplot(aes(prior_usage, name)) +
    geom_tile(aes(fill = prior_usage, alpha=really)) +
    geom_text(aes(label = prior_usages_text[prior_usage]), color="white", fontface="bold", size=8) +
    scale_fill_manual(values=prior_usage_colors) +
    scale_alpha_manual(values=c(no=0, yes=1)) +
    base_theme +
    empty_left_theme +
    base_scale_y + base_scale_x +
    horizontal_lines +
    theme(legend.position = "none") +
    ggtitle("Prior\ninformation")
prior_usage
prior_usage_width <- length(prior_usages)/2

# Trajectory components
trajectory_components_plot <- method_df_evaluated %>%
  gather(trajectory_type, can, !!trajectory_types, factor_key=TRUE) %>%
  select(trajectory_type, can, name) %>%
  mutate(name = factor(name, levels=method_order)) %>%
  drop_na() %>%
  ggplot() +
    geom_tile(aes(trajectory_type, name, fill = factor(can * as.numeric(trajectory_type)))) +
    scale_fill_manual(values = c("white", RColorBrewer::brewer.pal(8, name="Set2")), guide=FALSE) +
    base_theme +
    empty_left_theme +
    base_scale_y + base_scale_x +
    horizontal_lines +
    ggtitle("Detectable trajectory types")
trajectory_components_plot
trajectory_components_width <- length(trajectory_types)

# Maximal trajectory components
lighten <- function(color, factor=1.4){
  map_chr(color, function(color) {
    col <- col2rgb(color)
    col <- do.call(rgb2hsv, as.list(col))
    col[1] <- col[1] * 360
    col[2] <- 0.3
    col[3] <- 0.9
    colorspace::hex(do.call(colorspace::HSV, as.list(col)))
  })
}

maximal_trajectory_components_plot <- method_df_evaluated %>%
  gather(trajectory_type, can, !!trajectory_types, factor_key=TRUE) %>%
  group_by(name) %>%
  filter(can) %>%
  filter(row_number() == n()) %>%
  select(name, trajectory_type) %>%
  ungroup() %>%
  mutate(name = factor(name, levels=method_order)) %>%
  ggplot(aes("trajectory_type", name)) +
    geom_tile(aes(fill=as.character(trajectory_type))) +
    geom_text(aes(label=trajectory_type), hjust=0, vjust=0) +
    base_theme +
    base_scale_y + base_scale_x +
    horizontal_lines +
    theme(legend.position="none") +
    ggtitle("Trajectory\ntype") +
    scale_fill_manual(values=trajectory_type_background_colors)
maximal_trajectory_components_plot
maximal_trajectory_components_width <- 2

plotlist <- list(maximal_trajectory_components_plot, method_qc_individual, method_qc_overall, prior_usage)
widths <- c(maximal_trajectory_components_width+2, method_qc_individual_width, method_qc_overall_width, prior_usage_width)

method_characteristics <- plot_grid(plotlist=plotlist, nrow=1, align="h", rel_widths = widths, axis="bt", labels="auto")
method_characteristics

method_characteristics$width <- sum(widths)/2
method_characteristics$height <- length(method_order)/2

saveRDS(method_characteristics, figure_file("method_characteristics.rds"))




## PLOT1
method_characteristics <- read_rds(figure_file("method_characteristics.rds"))
ncitations_over_time <- read_rds(figure_file("ncitations_over_time.rds"))
platforms <- read_rds(figure_file("platforms.rds"))

bottom <- plot_grid(ncitations_over_time, platforms, nrow = 1, labels=c("e", "f"), rel_widths = c(2, 1))


figure <- plot_grid(
  method_characteristics,
  bottom,
  rel_heights = c(method_characteristics$width, 3),
  ncol=1
)
figure
save_plot(figure_file("figure_methods.pdf"), figure, base_width = method_characteristics$width, base_height= method_characteristics$height)




# replace trajectory types
library(xml2)

command <- glue::glue("inkscape {figure_file('figure_methods.pdf')} --export-plain-svg={figure_file('figure_methods.svg')}")
system(command)

file.remove(figure_file('figure_methods.pdf'))

svg_location <- figure_file('figure_methods.svg')
xml <- read_xml(svg_location)

aspect <- read_xml("analysis/figures/trajectory_types/mini/complex_fork.svg") %>% xml_root() %>% xml_attr("viewBox") %>% str_split(" ") %>% unlist() %>% tail(2) %>% as.numeric() %>% {.[[1]]/.[[2]]}

w <- 50
h <- w / aspect

trajectory_type_boxes <- readRDS(figure_file("trajectory_type_boxes.rds", "trajectory_types"))

map(trajectory_types, function(trajectory_type) {
  to_replace <- xml_find_all(xml, glue::glue("//svg:tspan[text()='{trajectory_type}']")) %>% xml_parent()
  transforms <- to_replace %>% xml_attr("transform")
  images <- map(transforms, function(transform) {
    node <- read_xml(glue::glue("<g><image /></g>"))
    node %>% xml_set_attr("transform", transform)
    node %>% xml_child() %>% xml_set_attrs(list(width=w, height=h, `xlink:href`=paste0("data:image/svg+xml;base64,", trajectory_type_boxes %>% filter(id == trajectory_type) %>% pull(base64)), transform=glue::glue("translate(-{w/2} -{h/2})")))
    node
  })
  replaced <- to_replace %>% xml_replace(images)
  if (length(replaced) == 0) {
    warning("STOOOOOOOOOOOOOOOOOOOOOOOOOOOPPP!!!")
  }
})

xml_root(xml) %>% xml_set_attr("xmlns:xlink", "http://www.w3.org/1999/xlink")

write(as.character(xml), file=figure_file('figure_methods.svg')); xml <- NULL
