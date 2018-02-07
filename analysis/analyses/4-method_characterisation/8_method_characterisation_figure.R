library(tidyverse)
library(dynalysis)
library(cowplot)

experiment("4-method_characterisation")


#   ____________________________________________________________________________
#   Create method aspects figure                                            ####
methods_evaluated <- read_rds(derived_file("methods_evaluated.rds"))
methods <- read_rds(derived_file("methods.rds"))
method_qc <- read_rds(derived_file("method_qc.rds"))

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
base_scale_y <- scale_y_discrete(drop=FALSE, expand=c(0, 0), labels=label_long)
base_scale_x <- scale_x_discrete(drop=FALSE, expand=c(0, 0), labels=label_long, name="")


##  ............................................................................
##  QC                                                                      ####
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
method_qc_category_width <- nrow(qc_categories)

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
method_qc_application_width <- nrow(qc_applications)

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


##  ............................................................................
##  Prior information                                                       ####
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


##  ............................................................................
##  Trajectory types                                                        ####
undirected_trajectory_type_order <- trajectory_types %>% filter(directedness == "undirected") %>% pull(id) %>% keep(~.!="unknown")

trajectory_components_plot <- methods_evaluated %>%
  gather(trajectory_type, can, !!undirected_trajectory_type_order, factor_key=TRUE) %>%
  select(trajectory_type, can, name) %>%
  mutate(name = factor(name, levels=method_order)) %>%
  drop_na() %>%
  ggplot() +
  geom_tile(aes(trajectory_type, name, fill = ifelse(can, trajectory_type, ""))) +
  scale_fill_manual(values = set_names(trajectory_types$color, trajectory_types$id), guide=FALSE) +
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
  rename(trajectory_type = maximal_trajectory_type) %>%
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


##  ............................................................................
##  Combining the figure                                                    ####

method_characteristics <- read_rds(figure_file("method_characteristics.rds"))
method_qc_ordering_plot <- read_rds(figure_file("method_qc_ordering.rds"))

figure <- plot_grid(
  method_characteristics,
  method_qc_ordering_plot,
  ncol=2,
  labels=c("", "e"),
  rel_widths = c(0.4, 0.6)
)
figure
save_plot(figure_file("figure_methods_overview.pdf"), figure, base_width = 17, base_height= 10)



##  ............................................................................
##  Postprocessing                                                          ####

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Replacing trajectory types                                              ####
library(xml2)

command <- glue::glue("inkscape {figure_file('figure_methods_overview.pdf')} --export-plain-svg={figure_file('figure_methods_overview.svg')}")
system(command)

file.remove(figure_file('figure_methods_overview.pdf'))

svg_location <- figure_file('figure_methods_overview.svg')
xml <- read_xml(svg_location)

aspect <- read_xml("analysis/figures/trajectory_types/mini/complex_fork.svg") %>% xml_root() %>% xml_attr("viewBox") %>% str_split(" ") %>% unlist() %>% tail(2) %>% as.numeric() %>% {.[[1]]/.[[2]]}

w <- 50
h <- w / aspect

trajectory_type_boxes <- readRDS(figure_file("trajectory_type_boxes.rds", "trajectory_types"))

map(unique(methods_evaluated$maximal_trajectory_type), function(trajectory_type) {
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
    warning("STOOOOOOOOOOOOOOOOOOOOOOOOOOOPPP!!! This will probabily crash your R session")
  }
})

xml_root(xml) %>% xml_set_attr("xmlns:xlink", "http://www.w3.org/1999/xlink")

write(as.character(xml), file=figure_file('method_characteristics.svg')); xml <- NULL

