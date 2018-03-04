library(cowplot)
library(tidyverse)
library(dynalysis)

experiment("6-developer_guidelines")
outputs_list <- read_rds(derived_file("outputs_postprocessed.rds", "5-optimise_parameters/3-evaluate_parameters"))
implementation_components <- read_rds(derived_file("implementation_components.rds", experiment_id = "4-method_characterisation"))
methods <- read_rds(derived_file("methods.rds", experiment_id = "4-method_characterisation"))
method_components <- implementation_components %>% left_join(methods %>% select(method_id, implementation_id)) %>% drop_na() %>% select(-implementation_id)
component_categories <- read_rds(derived_file("component_categories.rds", experiment_id = "4-method_characterisation"))
component_category_colors <- read_rds(derived_file("component_category_colors.rds", experiment_id = "4-method_characterisation"))

#   ____________________________________________________________________________
#   Component importance                                                    ####


# join categories as regular components
method_components_category <- method_components %>%
  left_join(component_categories, "component_id") %>%
  filter(!is.na(category)) %>%
  group_by(method_id, category) %>%
  summarise() %>%
  ungroup() %>%
  rename(component_id = category) %>%
  mutate(component_id = paste0("any ", component_id))

method_components <- bind_rows(
  method_components,
  method_components_category
)
component_categories <- bind_rows(
  component_categories,
  tibble(category = unique(component_categories$category), component_id = paste0("any ", category))
)

components <- tibble(component_id = unique(method_components$component_id))

method_scores <- outputs_list$outputs_summmethod_totals %>% filter(task_source == "mean") %>% {setNames(.$harm_mean, .$method_short_name)}
method_scores <- method_scores[intersect(methods$method_id, names(method_scores))]

method_features_components <- method_components %>%
  mutate(method_id = factor(method_id, levels=names(method_scores))) %>%
  group_by(method_id, component_id) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  mutate(contains=T) %>%
  spread("component_id", "contains", FALSE, drop=F) %>%
  slice(match(names(method_scores), method_id))

is_variable_feature <- function(x) sum(table(x) > 1) >= 4
method_features <- method_features_components %>%
  mutate(method_id = method_features_components$method_id)

method_data <- bind_cols(method_features, tibble(score = method_scores[method_features_components$method_id]))

components <- components %>%
  filter(component_id %in% colnames(method_features)) %>%
  mutate(n = colSums(method_features[,component_id])) %>%
  filter(n >= 4)

components$method_ids <- map(components$component_id, ~as.character(method_data$method_id[which(method_data[[.]])]))

##  ............................................................................
##  Wilcoxon component importance                                           ####
components$test <- map(components$component_id, function(component_id) {
  uses_component <- method_features[[component_id]]
  if(sum(uses_component) <= 2) {
    list(p.value = 1, estimate=c(0, 0))
  } else {
    # t.test(method_scores[uses_component], method_scores[!uses_component], conf.int = TRUE)
    wilcox.test(method_scores[uses_component], method_scores[!uses_component], conf.int = TRUE)
  }
})
components$p_value <- map_dbl(components$test, "p.value")
components$q_value <- components$p_value %>% p.adjust(method="BH")
# components$difference <- map_dbl(components$test, ~-diff(.$estimate))
components$difference <- map_dbl(components$test, "estimate")

##  ............................................................................
##  Random forest component importance                                      ####
rf <- randomForest::randomForest(method_features %>% select(-method_id), method_scores > median(method_scores), ntree=10000, importances=T)
importances <- randomForest::importance(rf) %>% as.data.frame() %>% rownames_to_column("feature_id") %>% arrange(-IncNodePurity) %>% as_tibble()
components$inc_node_purity <- importances %>% slice(match(components$component_id, feature_id)) %>% pull(IncNodePurity)



##  ............................................................................
##  Table                                                                   ####
wrap_pbox <- function(x, width=20, box_width="4cm") {
  paste0("\\pbox{", box_width, "}{", label_wrap(x, width, "\\\\[-0.5em]"), "}")
}
library(kableExtra)
table <- map(c("latex", "html"), function(format) {
  components_data <- components %>%
    left_join(component_categories, "component_id") %>%
    arrange(q_value) %>%
    mutate(
      category = cell_spec(
        category,
        format,
        color = component_category_colors[category]
      ),
      q_value = cell_spec(
        round(q_value, 5),
        format,
        background = spec_color(p_value,option="viridis",direction=-1),
        color = ifelse(p_value < mean(inc_node_purity), "black", "white")
      ),
      inc_node_purity = cell_spec(
        round(inc_node_purity, 3),
        format,
        background = spec_color(inc_node_purity,option="magma",direction=1),
        color = ifelse(inc_node_purity > mean(inc_node_purity), "black", "white")
      ),
      effect = cell_spec(
        ifelse(difference > 0, "Higher performance", "Lower performance"),
        format,
        color = ifelse(difference > 0, "#800000", "#000058")
      ),
      methods = cell_spec(
        map(method_ids, ~glue::collapse(setNames(methods$method_name, methods$method_id)[.], ", ")),
        format
      )
    ) %>%
    select(component_id, q_value, inc_node_purity, effect, methods, category)

  if(format == "latex") {
    components_data$methods <- map_chr(components_data$methods, wrap_pbox, width=20, box_width="20cm")
  }

  components_data %>%
    rename_all(label_long) %>%
    knitr::kable(format, escape=F) %>%
    kable_styling(bootstrap_options = "striped", latex_options = c("scale_down"))
}) %>% set_names(c("latex", "html"))
table

table %>% write_rds(figure_file("component_importance.rds"))

##  ............................................................................
##  Plot                                                                    ####

method_components_scores_data <- method_data %>%
  gather("component_id", "uses_component", -method_id, -score) %>%
  right_join(components, by="component_id") %>%
  left_join(component_categories, by="component_id") %>%
  arrange(q_value) %>%
  mutate(component_id = forcats::fct_inorder(component_id))
components <- components %>% mutate(effect = ifelse(difference < 0, "↘", "↗")) %>%
  mutate(component_id = factor(component_id, levels(method_components_scores_data$component_id)))


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

xmax <- length(levels(method_components_scores_data$component_id))+1
method_components_scores_dots <- method_components_scores_data %>% ggplot() +
  # geom_segment(aes(x = score, xend=score, y=0, yend=ymax), color="darkgrey") +
  geom_vline(aes(xintercept = as.numeric(component_id)), color="grey") +
  geom_violin(aes(as.numeric(component_id), score, group=component_id, fill=category), data=filter(method_components_scores_data, uses_component)) +
  ggrepel::geom_text_repel(
    aes(1, score, label=set_names(methods$method_name, methods$method_id)[method_id]),
    direction = "y",
    nudge_x = -1,
    data=
      method_components_scores_data %>%
      group_by(method_id) %>%
      filter(row_number() == 1),
    color="#888888",
    size=3,
    segment.size=0.2
  ) +
  geom_point(aes(as.numeric(component_id), score), color="#666666", shape=3, size=3) +
  geom_point(aes(as.numeric(component_id), score), color="black", size=3, data=filter(method_components_scores_data, uses_component)) +
  scale_x_continuous(
    "",
    breaks=seq_along(levels(method_components_scores_data$component_id)),
    labels=label_long(levels(method_components_scores_data$component_id)),
    limits=c(-1, xmax),
    expand=c(0,0)
  ) +
  geom_label(aes(as.numeric(component_id), max(method_components_scores_data$score) * 1.1, label=paste0(effect, " ", label_pvalue(q_value))), data=components, size=6) +
  scale_y_continuous() +
  scale_fill_manual(values=component_category_colors, label=label_long) +
  theme(legend.position="top", axis.text.x = element_text(angle=45, hjust=1))
method_components_scores_dots



method_components_scores_dots %>% ggsave(figure_file("method_components_scores.svg"),., width=12, height=10)
