library(tidyverse)
library(googlesheets)
library(dynalysis)
library(cowplot)

experiment("method_characteristics")

# Preprocessing the method_df file ---------------------------

# # If it's your first time running this script, run this:
# gs_auth()

script_file <- "analysis/analyses/4-method_characterization/scholar.py"
if (!file.exists(script_file)) {
  download.file("https://raw.githubusercontent.com/ckreibich/scholar.py/master/scholar.py", destfile = script_file)
}

num_citations_by_clusterid <- function(clusterid, scholar_file = script_file) {
  tryCatch({
    command <- paste0("python ", scholar_file, " -C ", clusterid, " --csv-header")
    output <- system(command, intern = T)
    tab <- readr::read_delim(paste(gsub("\n", " ", output), collapse = "\n"), delim = "|")
    sum(tab$num_citations)
  }, error = function(e) NA)
}

method_df <- gs_key("1Mug0yz8BebzWt8cmEW306ie645SBh_tDHwjVw4OFhlE") %>%
  gs_read(ws = "Software", col_types = cols(GScholarClusterID = "c"), skip = 1)

method_df$date <- method_df$Preprint
method_df$date[is.na(method_df$date)] <- method_df$PubDate[is.na(method_df$date)]

method_df <- method_df %>%
  mutate(citations = pbapply::pbsapply(cl=4, GScholarClusterID, num_citations_by_clusterid))

method_df_evaluated <- method_df %>%
  filter(!is.na(qc_score))

write_rds(method_df, derived_file("method_df.rds"))
write_rds(method_df_evaluated, derived_file("method_df_evaluated.rds"))


# --------------------------------------

method_df <- read_rds(derived_file("method_df.rds"))
method_df_evaluated <- read_rds(derived_file("method_df_evaluated.rds"))


# Trajectory components --------------------------
trajectory_components <- method_df_evaluated %>%
  select(name, trajectory_components, date) %>%
  mutate(
    segment = stringr::str_detect(trajectory_components, "segment"),
    fork = stringr::str_detect(trajectory_components, "fork"),
    convergence = stringr::str_detect(trajectory_components, "convergence"),
    loop = stringr::str_detect(trajectory_components, "loop"),
    n_forks = as.numeric(ifelse(fork, gsub(".*fork\\(.*,([0-9]|inf)\\).*", "\\1", trajectory_components), NA)),
    n_fork_paths = as.numeric(ifelse(fork, gsub(".*fork\\(([0-9]|inf),.*", "\\1", trajectory_components), NA)),
    n_convergences = as.numeric(ifelse(convergence, gsub(".*convergence\\(.*,([0-9]|inf)\\).*", "\\1", trajectory_components), NA)),
    n_convergence_paths = as.numeric(ifelse(convergence, gsub(".*convergence\\(([0-9]|inf),.*", "\\1", trajectory_components), NA))
  ) %>%
  mutate(
    linear = segment,
    single_bifurcation = fork,
    binary_tree = fork & n_forks > 1,
    single_multifurcation = fork & n_fork_paths > 2,
    non_binary_tree = binary_tree & single_multifurcation,
    cycle = loop,
    simple_graph = binary_tree & single_multifurcation & convergence
  )

write_rds(trajectory_components, derived_file("trajectory_components.rds"))

# Running the analyses ----------------------------
method_df <- read_rds(derived_file("method_df.rds"))
method_df_evaluated <- read_rds(derived_file("method_df_evaluated.rds"))

# Comparing number of citations, qc score, and time -------------------------------------
g1 <- ggplot(method_df_evaluated, aes(date, qc_score)) +
  geom_smooth() +
  geom_point() +
  ggrepel::geom_label_repel(aes(label = name), nudge_y = .25) +
  cowplot::theme_cowplot() +
  scale_y_continuous(breaks = c(0, 2, 4, 6, 8), limits = c(0, 8)) +
  labs(x = "Time", y = "QC score", title = "Code quality score over time")
g1
ggsave(figure_file("time_vs_qcscore.png"), g1, width = 16, height = 8)

g2 <- ggplot(method_df_evaluated, aes(date, Citations+1)) +
  geom_smooth() +
  geom_point() +
  ggrepel::geom_label_repel(aes(label = name), nudge_y = .25) +
  cowplot::theme_cowplot() +
  scale_y_log10() +
  labs(x = "Time", y = "Citations", title = "Citations over time")
g2
ggsave(figure_file("time_vs_citations.png"), g2, width = 16, height = 8)

g3 <- ggplot(method_df_evaluated, aes(Citations+1, qc_score)) +
  geom_smooth() +
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
platforms <- method_df %>% separate_rows(platform=platforms, sep=", ") %>%
  group_by(platforms) %>%
  summarise(quantity = n()) %>%
  arrange(quantity) %>%
  mutate(platform = ifelse(is.na(platforms), "Not available", platforms)) %>%
  mutate(platform = factor(platform, levels=rev(platform))) %>%
  ungroup() %>%
  mutate(pos = cumsum(quantity) - quantity/2) %>%
  ggplot(aes(1, quantity)) +
    geom_bar(aes(fill=platform), width = 1, stat="identity") +
    ggrepel::geom_label_repel(aes(1, pos, label=pritt("{platform}: {quantity}"), fill=platform), color="white", fontface = "bold", direction = "y", segment.alpha=0) +
    theme_void() +
    theme(legend.position="none") +
    coord_flip()
saveRDS(platforms, figure_file("platforms.rds"))

# Trajectory components over time -------------------------------------------
trajectory_components <- read_rds(derived_file("trajectory_components.rds"))

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
trajectory_types <- c("linear", "single_bifurcation", "binary_tree", "single_multifurcation", "non_binary_tree", "cycle", "simple_graph")

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
    geom_raster(aes(fill=score_fill)) +
    scale_fill_identity() +
    geom_text(aes(label = score_text), color="white", size=8, fontface = "bold") +
    theme(axis.text.x=rotated_axis_text_x) +
    base_theme +
    horizontal_lines +
    base_scale_y + base_scale_x +
    ggtitle("Implementation quality control")
method_qc_individual
method_qc_individual_width <- length(qcs)

method_qc_overall <- method_df_evaluated %>%
  select(name, qc_score) %>%
  mutate(name = factor(name, levels=method_order)) %>%
  ggplot(aes("qc_score", name)) +
    geom_raster(aes(fill = qc_score)) +
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
    geom_raster(aes(prior_id, name, fill = prior_usage)) +
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
prior_usages_text <- c(CanRoot = "✓", CanUse = "✓", Required = "!", No = "")
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
    geom_raster(aes(fill = prior_usage, alpha=really)) +
    geom_text(aes(label = prior_usages_text[prior_usage]), color="white", fontface="bold", size=8) +
    scale_fill_manual(values=prior_usage_colors) +
    scale_alpha_manual(values=c(no=0, yes=1)) +
    base_theme +
    empty_left_theme +
    base_scale_y + base_scale_x +
    horizontal_lines +
    theme(legend.position = "none") +
    ggtitle("Prior information")
prior_usage
prior_usage_width <- length(prior_usages)

# Trajectory components
trajectory_components <- readRDS(derived_file("trajectory_components.rds"))
trajectory_types <- c("linear", "single_bifurcation", "binary_tree", "single_multifurcation", "non_binary_tree", "cycle", "simple_graph")
trajectory_components_plot <- trajectory_components %>%
  gather(trajectory_type, can, !!trajectory_types, factor_key=TRUE) %>%
  select(trajectory_type, can, name) %>%
  mutate(name = factor(name, levels=method_order)) %>%
  drop_na() %>%
  ggplot() +
    geom_raster(aes(trajectory_type, name, fill = factor(can * as.numeric(trajectory_type)))) +
    scale_fill_manual(values = c("white", RColorBrewer::brewer.pal(8, name="Set2")), guide=FALSE) +
    base_theme +
    empty_left_theme +
    base_scale_y + base_scale_x +
    horizontal_lines +
    ggtitle("Detectable trajectory types")
trajectory_components_plot
trajectory_components_width <- length(trajectory_types)

method_characteristics <- plot_grid(method_qc_individual, method_qc_overall, prior_usage, trajectory_components_plot, nrow=1, align="h", rel_widths = c(method_qc_individual_width+4, method_qc_overall_width, prior_usage_width, trajectory_components_width), axis="bt", labels="auto")
method_characteristics

saveRDS(method_characteristics, figure_file("method_characteristics.rds"))




# Maximal trajectory
trajectory_components %>%
  gather(trajectory_type, can, !!trajectory_types, factor_key=TRUE) %>%
  group_by(name) %>%
  filter(can) %>%
  filter(row_number() == n()) %>%
  select(name, trajectory_type) %>%
  ungroup() %>%
  mutate(image_location = glue::glue("analysis/figures/trajectory_types/mini/{trajectory_type}.png")) %>%
  ggplot(aes(1, name)) +
    #ggimage::geom_image(aes(image=image_location), size=0.05) +
    geom_text(aes(label=trajectory_type))

ggsave("test.pdf")


command <- glue::glue("inkscape test.pdf --export-plain-svg=test.svg")
system(command)

svg_location <- "test.svg"
xml <- read_xml(svg_location)
xml_find_all(xml, "//svg:tspan[text()='linear']") %>% xml_parent() %>% xml_replace(glue::glue("<image xlink:href='analysis/figures/trajectory_types/mini/linear.svg' />"))
