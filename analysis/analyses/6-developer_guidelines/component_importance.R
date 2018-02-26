library(cowplot)
library(tidyverse)
library(dynalysis)

experiment("6-developer_guidelines")
outputs_list <- read_rds(derived_file("outputs_postprocessed.rds", "5-optimise_parameters/3-evaluate_parameters"))




#   ____________________________________________________________________________
#   Component importance                                                    ####
method_components <- read_rds(derived_file("method_components.rds", experiment_id = "4-method_characterisation"))
component_categories <- read_rds(derived_file("component_categories.rds", experiment_id = "4-method_characterisation"))

components <- tibble(component_id = unique(method_components$component_id))

method_scores <- outputs_list$outputs_summmethod_totals %>% filter(task_source == "mean") %>% {setNames(.$harm_mean, .$method_short_name)}
method_scores <- method_scores[names(method_scores) != "identity"]

method_features_components <- method_components %>%
  mutate(method_id = factor(method_id, levels=names(method_scores))) %>%
  group_by(method_id, component_id) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  mutate(contains=T) %>%
  spread("component_id", "contains", FALSE, drop=F) %>%
  slice(match(names(method_scores), method_id))

is_variable_feature <- function(x) sum(table(x) > 1) >= 2
method_features <- method_features_components %>%
  select_if(is_variable_feature) %>%
  mutate(method_id = method_features_components$method_id)

method_data <- bind_cols(method_features, tibble(score = method_scores[method_features_components$method_id]))

components <- components %>% filter(component_id %in% colnames(method_features))

##  ............................................................................
##  Wilcoxon component importance                                           ####
components$test <- map(components$component_id, function(component_id) {
  uses_component <- method_features[[component_id]]
  if(sum(uses_component) < 2) {
    list(p.value = 1, estimate=0)
  } else {
    wilcox.test(method_scores[uses_component], method_scores[!uses_component], conf.int = TRUE)
  }
})
components$p_value <- map_dbl(components$test, "p.value")
components$difference <- map_dbl(components$test, "estimate")

##  ............................................................................
##  Random forest component importance                                      ####
rf <- randomForest::randomForest(method_features %>% select(-method_id), method_scores > mean(method_scores), ntree=10000, importances=T)
importances <- randomForest::importance(rf) %>% as.data.frame() %>% rownames_to_column("feature_id") %>% arrange(-IncNodePurity) %>% as_tibble()
components$inc_node_purity <- importances %>% slice(match(components$component_id, feature_id)) %>% pull(IncNodePurity)



##  ............................................................................
##  Plot                                                                    ####

method_components_scores_data <- method_data %>%
  gather("component_id", "uses_component", -method_id, -score) %>%
  right_join(components %>% top_n(5, inc_node_purity), by="component_id") %>%
  left_join(component_categories, by="component_id") %>%
  arrange(inc_node_purity) %>%
  mutate(component_id = forcats::fct_inorder(component_id))


empty_left_theme <- theme(
  axis.text.y = element_blank(),
  axis.title.y = element_blank(),
  axis.line.y = element_blank(),
  axis.ticks.y = element_blank()
)
empty_bottom_theme <- theme(
  axis.text.x = element_blank(),
  axis.title.x = element_blank(),
  axis.line.x = element_blank(),
  axis.ticks.x = element_blank()
)
no_margin <- theme(plot.margin=margin())
margins_left <- theme(plot.margin=unit(c(0.4,-0.2,-0.4,0.2), "cm"))
margins_right <- theme(plot.margin=unit(c(0.4,0.2,0.4,0.2), "cm"))

ymax <- length(levels(method_components_scores_data$component_id))+1
method_components_scores_dots <- method_components_scores_data %>% ggplot() +
  # geom_segment(aes(x = score, xend=score, y=0, yend=ymax), color="darkgrey") +
  geom_hline(aes(yintercept = as.numeric(component_id)), color="grey") +
  geom_point(aes(score, as.numeric(component_id)), color="black", shape="|", size=3) +
  geom_point(aes(score, as.numeric(component_id), color=category), size=4, data=filter(method_components_scores_data, uses_component)) +
  ggrepel::geom_label_repel(
    aes(score, 1, label=method_id),
    direction = "x",
    nudge_y = -1,
    data=
      method_components_scores_data %>%
      group_by(method_id) %>%
      filter(row_number() == 1),
    color="darkgrey"
  ) +
  scale_y_continuous(
    "",
    breaks=seq_along(levels(method_components_scores_data$component_id)),
    labels=label_long(levels(method_components_scores_data$component_id)),
    limits=c(-1, ymax),
    expand=c(0,0)
  ) +
  scale_x_continuous(position="top") +
  theme(legend.position="top")
method_components_scores_dots


method_components_scores_tests <- method_components_scores_data %>%
  group_by(component_id) %>%
  filter(row_number() == 1) %>%
  ggplot() +
    geom_text(aes(0, as.numeric(component_id), label=round(inc_node_purity, 2)), hjust="left") +
    geom_text(aes(1, as.numeric(component_id), label=round(p_value, 2)), hjust="left") +
    geom_text(aes(2, as.numeric(component_id), label=ifelse(difference > 0, "↗", "↘")), hjust="right") +
    scale_x_continuous("", breaks=c(0, 1,2), labels=label_long(c("inc_node_purity", "pvalue", "tendency")), position="top") +
    scale_y_continuous("", limits=c(-1, ymax), expand=c(0,0)) +
    empty_left_theme +
    no_margin

method_components_scores_plot <- cowplot::plot_grid(method_components_scores_tests, method_components_scores_dots, rel_widths = c(0.2, 0.8),  ncol=2, align="hv", axis="bt")
method_components_scores_plot
method_components_scores_plot %>% write_rds(figure_file("method_components_scores_plot.rds"))

