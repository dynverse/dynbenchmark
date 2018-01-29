library(tidyverse)
library(dynalysis)
library(cowplot)

experiment("method_characteristics")
source("analysis/analyses/4-method_characterization/0_common.R")

methods_evaluated <- read_rds(derived_file("methods_evaluated.rds"))
methods <- read_rds(derived_file("methods.rds"))
method_qc <- read_rds(derived_file("method_qc.rds"))

undirected_trajectory_type_order <- trajectory_type_nodes %>% filter(directedness == "undirected") %>% pull(id) %>% keep(~.!="unknown")

#######################"

# Comparing number of citations, qc score, and time -------------------------------------
g1 <- ggplot(methods_evaluated, aes(date, qc_score)) +
  geom_smooth(span=2) +
  geom_point() +
  ggrepel::geom_label_repel(aes(label = name)) +
  cowplot::theme_cowplot() +
  labs(x = "Time", y = "QC score", title = "Code quality score over time")
g1

write_rds(g1, figure_file("ncitations_over_time.rds"))
ggsave(figure_file("time_vs_qcscore.png"), g1, width = 16, height = 8)

g2 <- ggplot(methods_evaluated, aes(date, Citations+1)) +
  geom_smooth(method="lm") +
  geom_point() +
  ggrepel::geom_label_repel(aes(label = name), nudge_y = .25) +
  cowplot::theme_cowplot() +
  scale_y_log10() +
  labs(x = "Time", y = "Citations", title = "Citations over time")
g2
ggsave(figure_file("time_vs_citations.png"), g2, width = 16, height = 8)

g3 <- ggplot(methods_evaluated, aes(Citations+1, qc_score)) +
  geom_smooth(method="lm") +
  geom_point() +
  ggrepel::geom_label_repel(aes(label = name)) +
  cowplot::theme_cowplot() +
  scale_x_log10() +
  labs(x = "Citations", y = "QC score", title = "QC score over # citations")
g3
ggsave(figure_file("citations_vs_qcscore.png"), g3, width = 8, height = 8)


# Publication timeline of methods -----------------
method_publication_data <- methods %>% filter(is_ti) %>%
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
platforms <- methods_evaluated %>% separate_rows(platform=platforms, sep=", ") %>%
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
trajectory_components <- methods %>% filter(is_ti)
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
  gather(trajectory_type, can_trajectory_type, !!undirected_trajectory_type_order) %>%
  mutate(trajectory_type = factor(trajectory_type, levels=undirected_trajectory_type_order)) %>%
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
method_order <- methods_evaluated %>% arrange(qc_score) %>% pull(name)
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
base_scale_x <- scale_x_discrete(drop=FALSE, expand=c(0, 0), labels=labels, name="")



# QC
method_qc_scores <- read_rds(derived_file("method_qc_scores.rds"))
method_qc_category_scores <- read_rds(derived_file("method_qc_category_scores.rds"))
method_qc_application_scores <- read_rds(derived_file("method_qc_application_scores.rds"))
method_qc_category <- method_qc_category_scores %>%
  mutate(name = factor(method_id, levels=method_order)) %>%
  ggplot(aes(category, name)) +
    geom_tile(aes(fill=qc_score)) +
    viridis::scale_fill_viridis(option="A", limits=c(0, 1)) +
    theme(axis.text.x=rotated_axis_text_x) +
    base_theme +
    empty_left_theme +
    horizontal_lines +
    base_scale_y + base_scale_x +
    ggtitle("Implementation\nquality control") +
    theme(legend.position = "none")
method_qc_category
method_qc_category_width <- length(categories)

method_qc_application <- method_qc_application_scores %>%
  mutate(name = factor(method_id, levels=method_order)) %>%
  ggplot(aes(application, name)) +
  geom_tile(aes(fill=score)) +
  viridis::scale_fill_viridis(option="A", limits=c(0, 1)) +
  theme(axis.text.x=rotated_axis_text_x) +
  base_theme +
  empty_left_theme +
  horizontal_lines +
  base_scale_y + base_scale_x +
  theme(legend.position = "none")
method_qc_application
method_qc_application_width <- length(applications)

method_qc_overall <- method_qc_scores %>%
  mutate(name = factor(method_id, levels=method_order)) %>%
  ggplot(aes("qc_score", name)) +
    geom_tile(aes(fill = qc_score)) +
    geom_text(aes(label = round(qc_score*10, 1), color=ifelse(qc_score > 0.76, "black", "white"))) +
    viridis::scale_fill_viridis(option="A", limits=c(0, 1)) +
    scale_color_identity() +
    base_theme +
    empty_left_theme +
    horizontal_lines +
    base_scale_y + base_scale_x +
    theme(legend.position = "none")
method_qc_overall
method_qc_overall_width <- 1

# Prior information
priors <- c("start_cell_ids", "grouping_assignment", "time", "timecourse",	"end_cell_ids",	"n_end_states","n_branches", "marker_gene_ids")
prior_usage_colors <- c("?" = "#AAAAAA", "no" = "#EEEEEE", "required" = "#FF4136", "can_use" = "#0074D9", "can_root" = "#39CCCC")
prior_information <- methods_evaluated %>%
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


prior_usages <- c("can_root", "can_use", "required")
prior_usages_text <- c(CanRoot = "", CanUse = "", Required = "!", No = "")
prior_usage <- methods_evaluated %>%
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
    theme(legend.position = "none", plot.margin = unit(c(2, 3, 0.5, 0), "lines")) +
    ggtitle("Prior\ninformation")
prior_usage
prior_usage_width <- length(prior_usages)/2

# Trajectory components
trajectory_components_plot <- methods_evaluated %>%
  gather(trajectory_type, can, !!undirected_trajectory_type_order, factor_key=TRUE) %>%
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
trajectory_components_width <- length(undirected_trajectory_type_order)

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

maximal_trajectory_components_plot <- methods_evaluated %>%
  gather(trajectory_type, can, !!undirected_trajectory_type_order, factor_key=TRUE) %>%
  group_by(name) %>%
  filter(can) %>%
  filter(row_number() == n()) %>%
  select(name, trajectory_type) %>%
  ungroup() %>%
  mutate(name = factor(name, levels=method_order)) %>%
  ggplot(aes("trajectory_type", name)) +
    geom_tile(fill="white") +
    geom_text(aes(label=trajectory_type), hjust=0, vjust=0) +
    base_theme +
    base_scale_y + base_scale_x +
    horizontal_lines +
    theme(legend.position="none") +
    ggtitle("Trajectory\ntype")
maximal_trajectory_components_plot
maximal_trajectory_components_width <- 2

plotlist <- list(maximal_trajectory_components_plot, method_qc_overall, method_qc_category, method_qc_application, prior_usage)
widths <- c(maximal_trajectory_components_width+2, method_qc_overall_width, method_qc_category_width/2, method_qc_application_width/2, prior_usage_width+2)

method_characteristics <- plot_grid(plotlist=plotlist, nrow=1, align="h", rel_widths = widths, axis="bt", labels="auto")
method_characteristics

method_characteristics$width <- sum(widths)/2
method_characteristics$height <- length(method_order)/2

saveRDS(method_characteristics, figure_file("method_characteristics.rds"))


## Small history
method_small_history <- methods %>%
  filter(is_ti) %>%
  mutate(
    maximal_trajectory_type = ifelse(is.na(maximal_trajectory_type), "unknown", as.character(maximal_trajectory_type)),
    y = runif(n(), 0, 0.0001),
    fontface = ifelse(evaluated, "plain", "plain"),
    size = ifelse(evaluated, 3.5, 3.5)
  ) %>%
  mutate() %>%
  arrange(-y) %>%
  ggplot(aes(date, y)) +
  ggrepel::geom_label_repel(aes(color=maximal_trajectory_type, label=name, fontface=fontface, size=size), direction="y", max.iter=10000, ylim=c(0, NA), force=10, min.segment.length = 0) +
  scale_color_manual(values=trajectory_type_colors) +
  scale_fill_manual(values=trajectory_type_background_colors) +
  scale_alpha_manual(values=c(`TRUE`=1, `FALSE`=0.6)) +
  scale_x_date(label_long("publishing_date"), limits=c(as.Date("2014-01-01"), as.Date("2018-01-01")), date_breaks="1 year", date_labels="%Y") +
  scale_y_continuous(NULL, breaks=NULL, limits=c(0, 1), expand=c(0, 0)) +
  scale_size_identity() +
  theme(legend.position="none")
method_small_history

ggsave(figure_file("method_small_history.svg"), method_small_history, height = 3.5, width = 10)

# move lines to back
library(xml2)
svg <- xml2::read_xml(figure_file("method_small_history.svg"))

children <- svg %>% xml_children()
front <- xml_name(children) %in% c("line")
new <- children[!front]
for (node in children[front]) {
  new %>% xml_add_sibling(node, .copy = FALSE, where=c("before"))
}
children[xml_name(children) == "rect"] %>% xml_remove()
xml2::write_xml(svg, figure_file("method_small_history.svg"))

###

method_small_distribution <- methods %>%
  filter(is_ti) %>%
  gather(trajectory_type, can_handle, !!undirected_trajectory_type_order) %>%
  mutate(trajectory_type = factor(trajectory_type, levels=undirected_trajectory_type_order)) %>%
  filter(can_handle) %>%
  group_by(trajectory_type) %>%
  count() %>%
  ggplot(aes(trajectory_type, n)) +
    geom_bar(aes(fill=trajectory_type, color=trajectory_type), stat="identity", width=0.95) +
    geom_text(aes(label=n), vjust=0) +
    # geom_hline(yintercept = sum(methods$is_ti, na.rm=TRUE), line) +
    scale_fill_manual(values=trajectory_type_background_colors) +
    scale_color_manual(values=trajectory_type_colors) +
    scale_y_continuous(expand=c(0, 2)) +
    theme(legend.position = "None")
method_small_distribution
ggsave(figure_file("method_small_distribution.svg"), method_small_distribution, height = 2, width = 5)


methods %>%
  filter(is_ti) %>%
  gather(trajectory_type, can_handle, !!undirected_trajectory_type_order) %>%
  mutate(trajectory_type = factor(trajectory_type, levels=undirected_trajectory_type_order)) %>%
  filter(can_handle) %>%
  group_by(trajectory_type) %>%
  count()

### Table
date_cutoff <- as.Date("2017-05-01")
date_filter <- methods$date > date_cutoff
methods$non_inclusion_reasons_split[date_filter] <- map(methods$non_inclusion_reasons_split[date_filter], c, "date") %>% map(unique)

methods$non_inclusion_reasons_footnotes <- methods$non_inclusion_reasons_split %>%
  map(function(non_inclusion_reasons) {
    slice(non_inclusion_reasons_footnotes, match(non_inclusion_reasons, id))$footnote
  })

methods %>%
  filter(is_ti) %>%
  arrange(date) %>%
  mutate(
    included = ifelse(evaluated, "✓", map(non_inclusion_reasons_footnotes, paste0, collapse=" ")),
    date = strftime(date, "%d/%m/%Y"),
    maximal_trajectory_type = label_long(maximal_trajectory_type)
  ) %>%
  select(name, date, maximal_trajectory_type, included) %>%
  rename_all(label_long) %>%
  saveRDS(figure_file("methods_table.rds"))

