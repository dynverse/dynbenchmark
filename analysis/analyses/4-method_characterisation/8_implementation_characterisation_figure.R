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
##  Create svg replaces                                                     ####
# for priors
implementations_evaluated$prior_mini_id <- group_indices(group_by_at(implementations_evaluated, vars(!!priors$prior_id)))
priors_mini <- implementations_evaluated %>%
  group_by_at(vars(!!priors$prior_id, prior_mini_id)) %>%
  summarise() %>%
  ungroup() %>%
  mutate(method_priors_id = row_number()) %>%
  gather(prior_id, prior_usage, -method_priors_id, -prior_mini_id) %>%
  {split(., .$method_priors_id)} %>%
  map_chr(generate_prior_mini) %>%
  {tibble(prior_mini_id = names(.), svg=.)}

# for trajectory types
trajectory_types_mini <- map(trajectory_types$id, ~xml2::read_xml(figure_file(paste0("mini/", ., ".svg"), "trajectory_types"))) %>%
  map_chr(as.character) %>%
  {tibble(trajectory_type = trajectory_types$id, svg=.)}

minis <- bind_rows(priors_mini,trajectory_types_mini ) %>% create_replacers()

##  ............................................................................
##  QC                                                                      ####
implementation_qc_scores <- read_rds(derived_file("implementation_qc_scores.rds"))
implementation_qc_category_scores <- read_rds(derived_file("implementation_qc_category_scores.rds"))
implementation_qc_application_scores <- read_rds(derived_file("implementation_qc_application_scores.rds"))
implementation_qc_category <- implementation_qc_category_scores %>%
  filter(implementation_id %in% implementation_order) %>%
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
  filter(implementation_id %in% implementation_order) %>%
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
  filter(implementation_id %in% implementation_order) %>%
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
prior_information <- implementations_evaluated %>%
  gather(prior_id, prior_usage, !!priors$prior_id, factor_key=TRUE) %>%
  select(prior_id, prior_usage, implementation_id) %>%
  mutate(implementation_id = factor(implementation_id, levels=implementation_order)) %>%
  drop_na() %>%
  ggplot() +
  geom_tile(aes(prior_id, implementation_id, fill = prior_usage)) +
  geom_text(aes(prior_id, implementation_id, label=ifelse(prior_usage == "No", "-", prior_usage))) +
  scale_fill_manual(values=setNames(prior_usages$color, prior_usages$prior_usage)) +
  base_theme +
  empty_left_theme +
  horizontal_lines +
  base_scale_y + base_scale_x +
  theme(legend.position = "none") +
  ggtitle("Prior information")
prior_information



priors_mini <- implementations_evaluated %>%
  group_by_at(vars(!!priors$prior_id, prior_mini_id)) %>%
  summarise() %>%
  ungroup() %>%
  mutate(method_priors_id = row_number()) %>%
  gather(prior_id, prior_usage, -method_priors_id, -prior_mini_id) %>%
  {split(., .$method_priors_id)} %>%
  map_chr(generate_prior_mini) %>%
  {tibble(prior_mini_id = names(.), svg=.)}

priors_rect <- implementations_evaluated %>%
  mutate(implementation_id = factor(implementation_id, levels=implementation_order)) %>%
  select(implementation_id, prior_mini_id) %>%
  drop_na() %>%
  ggplot(aes("", implementation_id)) +
  geom_tile(aes(alpha=as.character(prior_mini_id)), fill="#ABCDEF") +
  scale_alpha_manual(values=set_names(minis$replace_id, minis$prior_mini_id)) +
  base_theme +
  empty_left_theme +
  horizontal_lines +
  base_scale_y + base_scale_x +
  theme(legend.position = "none") +
  ggtitle("Prior information")
priors_rect
priors_rect_width <- 2

##  ............................................................................
##  Trajectory types                                                        ####
directed_trajectory_type_order <- trajectory_types %>% filter(directedness == "directed") %>% pull(id) %>% keep(~.!="unknown")

trajectory_components_plot <- implementations_evaluated %>%
  gather(trajectory_type, can, !!directed_trajectory_type_order, factor_key=TRUE) %>%
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
trajectory_components_width <- length(directed_trajectory_type_order)

# Maximal trajectory components
maximal_trajectory_components_plot <- implementations_evaluated %>%
  rename(trajectory_type = maximal_trajectory_type) %>%
  select(implementation_id, trajectory_type) %>%
  ungroup() %>%
  mutate(implementation_id = factor(implementation_id, levels=implementation_order)) %>%
  ggplot(aes("trajectory_type", implementation_id)) +
  geom_tile(aes(alpha = trajectory_type), fill="#ABCDEF") +
  scale_alpha_manual(values = set_names(minis$replace_id, minis$trajectory_type)) +
  base_theme +
  base_scale_y + base_scale_x +
  scale_y_discrete(label=function(x) implementation_id_to_name[x]) +
  horizontal_lines +
  theme(legend.position="none") +
  ggtitle("Trajectory\ntype")
maximal_trajectory_components_plot
maximal_trajectory_components_width <- 1

plotlist <- list(maximal_trajectory_components_plot, implementation_qc_overall, implementation_qc_category, implementation_qc_application, priors_rect)
widths <- c(maximal_trajectory_components_width+2, implementation_qc_overall_width, implementation_qc_category_width/2, implementation_qc_application_width/2, priors_rect_width)

implementation_overview <- plot_grid(plotlist=plotlist, nrow=1, align="h", rel_widths = widths, axis="bt", labels="auto")
implementation_overview

implementation_overview$width <- sum(widths)*0.8
implementation_overview$height <- length(implementation_order)/2*0.8

saveRDS(implementation_overview, figure_file("implementation_overview.rds"))

save_plot(figure_file("implementation_overview.svg"), implementation_overview, base_width = implementation_overview$width, base_height= implementation_overview$height)

##  ............................................................................
##  Postprocessing                                                          ####
replace(xml2::read_xml(figure_file("implementation_overview.svg")), minis) %>% xml2::write_xml(figure_file("implementation_overview.svg"))

