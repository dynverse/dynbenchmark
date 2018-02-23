library(tidyverse)
library(dynalysis)
library(cowplot)

experiment("4-method_characterisation")

implementation_id_to_name <- setNames(implementations$implementation_name, implementations$implementation_id)


#   ____________________________________________________________________________
#   Create implementation aspects figure                                            ####
implementations_evaluated <- read_rds(derived_file("implementations_evaluated.rds"))
implementations <- read_rds(derived_file("implementations.rds"))
implementation_qc <- read_rds(derived_file("implementation_qc.rds"))

implementation_order <- implementations_evaluated %>% arrange(qc_score) %>% pull(implementation_id)
horizontal_lines <- geom_hline(aes(yintercept = y+0.5), alpha=0.2, data=tibble(y=seq_along(implementation_order)))

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
implementation_qc_scores <- read_rds(derived_file("implementation_qc_scores.rds"))
implementation_qc_category_scores <- read_rds(derived_file("implementation_qc_category_scores.rds"))
implementation_qc_application_scores <- read_rds(derived_file("implementation_qc_application_scores.rds"))
implementation_qc_category <- implementation_qc_category_scores %>%
  mutate(implementation_id = factor(implementation_id, levels=implementation_order)) %>%
  ggplot(aes(category, implementation_id)) +
  geom_tile(aes(fill=qc_score)) +
  viridis::scale_fill_viridis(option="A", limits=c(0, 1)) +
  theme(axis.text.x=rotated_axis_text_x) +
  base_theme +
  empty_left_theme +
  horizontal_lines +
  base_scale_y + base_scale_x +
  ggtitle("Implementation\nquality control") +
  theme(legend.position = "none")
implementation_qc_category
implementation_qc_category_width <- nrow(qc_categories)

implementation_qc_application <- implementation_qc_application_scores %>%
  mutate(implementation_id = factor(implementation_id, levels=implementation_order)) %>%
  ggplot(aes(application, implementation_id)) +
  geom_tile(aes(fill=score)) +
  viridis::scale_fill_viridis(option="A", limits=c(0, 1)) +
  theme(axis.text.x=rotated_axis_text_x) +
  base_theme +
  empty_left_theme +
  horizontal_lines +
  base_scale_y + base_scale_x +
  theme(legend.position = "none")
implementation_qc_application
implementation_qc_application_width <- nrow(qc_applications)

implementation_qc_overall <- implementation_qc_scores %>%
  mutate(implementation_id = factor(implementation_id, levels=implementation_order)) %>%
  ggplot(aes("qc_score", implementation_id)) +
  geom_tile(aes(fill = qc_score)) +
  geom_text(aes(label = round(qc_score*10, 1), color=ifelse(qc_score > 0.76, "black", "white"))) +
  viridis::scale_fill_viridis(option="A", limits=c(0, 1)) +
  scale_color_identity() +
  base_theme +
  empty_left_theme +
  horizontal_lines +
  base_scale_y + base_scale_x +
  theme(legend.position = "none")
implementation_qc_overall
implementation_qc_overall_width <- 1


##  ............................................................................
##  Prior information                                                       ####
priors <- c("start_cell_ids", "grouping_assignment", "time", "timecourse",	"end_cell_ids",	"n_end_states","n_branches", "marker_gene_ids")
prior_usage_colors <- c("?" = "#AAAAAA", "no" = "#EEEEEE", "required" = "#FF4136", "can_use" = "#0074D9", "can_root" = "#39CCCC")
prior_usage_colors[["required_default"]] <- prior_usage_colors[["required"]]
prior_information <- implementations_evaluated %>%
  gather(prior_id, prior_usage, !!priors, factor_key=TRUE) %>%
  select(prior_id, prior_usage, implementation_id) %>%
  mutate(implementation_id = factor(implementation_id, levels=implementation_order)) %>%
  drop_na() %>%
  ggplot() +
  geom_tile(aes(prior_id, implementation_id, fill = prior_usage)) +
  geom_text(aes(prior_id, implementation_id, label=ifelse(prior_usage == "No", "-", prior_usage))) +
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
prior_usage <- implementations_evaluated %>%
  gather(prior_id, prior_usage, !!priors, factor_key=TRUE) %>%
  select(prior_id, prior_usage, implementation_id) %>%
  group_by(implementation_id, prior_usage) %>%
  summarise() %>%
  ungroup() %>%
  mutate(
    prior_usage = factor(prior_usage, levels=prior_usages),
    implementation_id = factor(implementation_id, levels=implementation_order)
  ) %>%
  drop_na() %>%
  mutate(really = TRUE) %>%
  complete(implementation_id, prior_usage) %>%
  mutate(really = ifelse(is.na(really), "no", "yes")) %>%
  ggplot(aes(prior_usage, implementation_id)) +
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
undirected_trajectory_type_order <- trajectory_types %>% filter(directedness == "directed") %>% pull(id) %>% keep(~.!="unknown")

trajectory_components_plot <- implementations_evaluated %>%
  gather(trajectory_type, can, !!undirected_trajectory_type_order, factor_key=TRUE) %>%
  select(trajectory_type, can, implementation_id) %>%
  mutate(implementation_id = factor(implementation_id, levels=implementation_order)) %>%
  drop_na() %>%
  ggplot() +
  geom_tile(aes(trajectory_type, implementation_id, fill = ifelse(can, trajectory_type, ""))) +
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

maximal_trajectory_components_plot <- implementations_evaluated %>%
  rename(trajectory_type = maximal_trajectory_type) %>%
  select(implementation_id, trajectory_type) %>%
  ungroup() %>%
  mutate(implementation_id = factor(implementation_id, levels=implementation_order)) %>%
  ggplot(aes("trajectory_type", implementation_id)) +
  geom_tile(fill="white") +
  geom_text(aes(label=trajectory_type), hjust=0, vjust=0) +
  base_theme +
  base_scale_y + base_scale_x +
  scale_y_discrete(label=function(x) implementation_id_to_name[x]) +
  horizontal_lines +
  theme(legend.position="none") +
  ggtitle("Trajectory\ntype")
maximal_trajectory_components_plot
maximal_trajectory_components_width <- 2

plotlist <- list(maximal_trajectory_components_plot, implementation_qc_overall, implementation_qc_category, implementation_qc_application, prior_usage)
widths <- c(maximal_trajectory_components_width+3, implementation_qc_overall_width, implementation_qc_category_width/2, implementation_qc_application_width/2, prior_usage_width+2)

implementation_characterisation <- plot_grid(plotlist=plotlist, nrow=1, align="h", rel_widths = widths, axis="bt", labels="auto")
implementation_characterisation

implementation_characterisation$width <- sum(widths)/2
implementation_characterisation$height <- length(implementation_order)/2

saveRDS(implementation_characterisation, figure_file("implementation_characterisation.rds"))


##  ............................................................................
##  Combining the figure                                                    ####

implementation_characterisation <- read_rds(figure_file("implementation_characterisation.rds"))
implementation_qc_ordering_plot <- read_rds(figure_file("implementation_qc_ordering.rds"))

figure <- plot_grid(
  implementation_characterisation,
  implementation_qc_ordering_plot,
  ncol=2,
  labels=c("", "e"),
  rel_widths = c(0.4, 0.6)
)
figure
save_plot(figure_file("implementations_characterisation.pdf"), figure, base_width = 17, base_height= 12)



##  ............................................................................
##  Postprocessing                                                          ####

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Replacing trajectory types                                              ####
library(xml2)

command <- glue::glue("inkscape {figure_file('implementations_characterisation.pdf')} --export-plain-svg={figure_file('implementations_characterisation.svg')}")
system(command)

file.remove(figure_file('implementations_characterisation.pdf'))

svg_location <- figure_file('implementations_characterisation.svg')
xml <- read_xml(svg_location)

aspect <- read_xml("analysis/figures/trajectory_types/mini/complex_fork.svg") %>% xml_root() %>% xml_attr("viewBox") %>% str_split(" ") %>% unlist() %>% tail(2) %>% as.numeric() %>% {.[[1]]/.[[2]]}

w <- 50
h <- w / aspect

trajectory_type_boxes <- readRDS(figure_file("trajectory_type_boxes.rds", "trajectory_types"))

map(unique(implementations_evaluated$maximal_trajectory_type), function(trajectory_type) {
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

write(as.character(xml), file=figure_file('implementations_characterisation.svg')); xml <- NULL

