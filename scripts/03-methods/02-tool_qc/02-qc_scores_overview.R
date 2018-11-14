#' Create an overview figure of the quality control

library(dynbenchmark)
library(tidyverse)
library(patchwork)

experiment("03-methods/02-tool_qc")

tool_qc <- read_rds(result_file("tool_qc.rds", "03-methods"))
tools_evaluated <- read_rds(result_file("tools_evaluated.rds", "03-methods"))
qc_checks <- readRDS(result_file("qc_checks.rds", "03-methods"))

# the order of the tools is determined based on overall qc score
tool_order <- tools_evaluated %>%
  arrange(-qc_score) %>%
  filter(!is.na(qc_score)) %>%
  pull(tool_id)
tools_evaluated <- tools_evaluated %>% mutate(tool_id = factor(tool_id, tool_order))

label_tool <- function(tool_id) {tools_evaluated$tool_name[match(tool_id, tools_evaluated$tool_id)]}

color_scale_qc <- grDevices::colorRampPalette(rev(RColorBrewer::brewer.pal(9, "Greens")[-1] %>% c("#00250f")))(101) %>%
  scale_fill_gradientn(
    "Usability score",
    colours = .,
    guide=guide_colorbar(title.position = "top", title.hjust=0.5, barwidth = unit(2, "inches"))
  )

# overall ordering plot of the tools
plot_tool_ordering <- tools_evaluated %>%
  mutate(tool_label = label_tool(tool_id)) %>%
  ggplot(aes(as.numeric(tool_id), qc_score)) +
  geom_bar(aes(fill=qc_score), stat="identity") +
  geom_text(aes(y=0.015, label=tool_label, color=ifelse(qc_score > 0.5, "black", "white")), angle=90, hjust=0, size=3) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_y_continuous(NULL, expand=c(0, 0), limits=c(0, 1)) +
  scale_x_continuous("", breaks=NULL, expand = c(0, 0)) +
  color_scale_qc +
  # viridis::scale_fill_viridis(label_long("qc_score"), option="D", guide=guide_colorbar(title.position = "top", title.hjust=0.5, barwidth = unit(2, "inches"))) +
  scale_color_identity() +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf) +
  theme_pub() +
  theme(legend.position="top")

plot_tool_ordering
plot_tool_ordering %>% write_rds(result_file("tool_ordering.rds"), compress = "xz")

##  ............................................................................
##  Heatmap of the individual aspects of each methods                       ####

# calculate the vertical positions of each aspect & check, based on its weight
qc_checks <- tool_qc %>%
  filter(tool_id == "scorpius") %>%
  group_by(category) %>%
  mutate(normalised_weight = weight/sum(weight)) %>%
  ungroup()


check_positions <- qc_checks %>%
  mutate(
    check_width = item_weight * normalised_weight,
    new_category = c(0, diff(as.numeric(factor(category))) != 0) * 0.1,
    check_end = cumsum(check_width) + cumsum(new_category),
    check_start = c(0, head(cumsum(check_width), -1) + tail(cumsum(new_category), -1)),
    check_mid = map2_dbl(check_end, check_start, ~mean(c(.x, .y)))
  ) %>%
  select(check_id, aspect_id, category, check_start, check_end, check_mid)

aspect_positions <- check_positions %>%
  group_by(aspect_id) %>%
  summarise(
    aspect_end = max(check_end),
    aspect_start = min(check_start),
    aspect_mid = map2_dbl(aspect_end, aspect_start, ~mean(c(.x, .y)))
  ) %>%
  left_join(qc_checks %>% select(aspect_id, name) %>% group_by(aspect_id) %>% slice(1), "aspect_id")

category_positions <- check_positions %>%
  group_by(category) %>%
  summarise(
    category_end = max(check_end),
    category_start = min(check_start),
    category_mid = map2_dbl(category_start, category_end, ~mean(c(.x, .y)))
  )

hline_check <- geom_hline(aes(yintercept=check_end), data=check_positions, color="#EEEEEE", alpha=0.25)
hline_aspect <- geom_hline(aes(yintercept=aspect_end), data=aspect_positions, color="#EEEEEE")

tool_qc_checks <- tool_qc %>%
  mutate(tool_id = factor(tool_id, tool_order)) %>%
  left_join(check_positions, by=c("check_id","aspect_id")) %>%
  mutate(answer = ifelse(is.na(answer), 0, answer))

plot_individual_checks <- tool_qc_checks %>%
  ggplot() +
  geom_rect(aes(ymax=check_start, ymin=check_end, xmin=as.integer(tool_id), xmax=as.integer(tool_id) + 1, alpha=answer), fill = "#444444") +
  hline_check + hline_aspect +
  geom_vline(aes(xintercept = as.integer(tool_id)), color="white", alpha=0.2) +
  scale_y_reverse(NULL, breaks = aspect_positions$aspect_mid, labels = aspect_positions$name, expand = c(0, 0)) +
  scale_x_continuous("", breaks = seq_along(levels(tool_qc_checks$tool_id))+0.5, labels = label_tool(levels(tool_qc_checks$tool_id)), expand = c(0, 0), position="right") +
  # scale_x_continuous("", breaks=NULL, expand = c(0, 0)) +
  scale_fill_brewer(palette = "Set1") +
  # scale_fill_manual("Category", values=set_names(qc_categories$color, qc_categories$category), labels=set_names(qc_categories$label, qc_categories$category), guide=guide_legend(ncol=2)) +
  scale_alpha_continuous("Score", guide=guide_legend(ncol=5, label.position = "bottom")) +
  theme_pub() +
  theme(
    axis.text.x = element_text(angle = 60, vjust = 1, hjust=1),
    axis.text.y = element_text(size=8), legend.position="bottom",
    axis.line.y = element_blank()
  )

plot_individual_checks


##  ............................................................................
##  Add categories to heatmap                                               ####

plot_category_labels <- category_positions %>%
  ggplot(aes(0, category_mid)) +
  # geom_rect(aes(xmin = -0.1, ymin = category_start, xmax = 0.1, ymax = category_end), fill = "black", alpha = 0.2) +
  geom_segment(aes(x = 0.05, y = category_start, xend = 0.05, yend = category_end), color = "black") +
  geom_text(aes(label = label_long(category)), angle = 90) +
  scale_x_continuous(limits = c(-0.1, 0.1), expand = c(0, 0)) +
  scale_y_reverse(expand = c(0, 0)) +
  theme_pub() +
  theme_void()

wrap_plots(plot_category_labels, plot_individual_checks, nrow = 1)


##  ............................................................................
##  Highlight some difficult checks                                         ####

# calculate difficulte based on average score
check_scores <- tool_qc_checks %>% group_by(check_id) %>% summarise(score = mean(answer))
aspect_scores <- tool_qc_checks %>% group_by(aspect_id) %>% summarise(score = mean(answer))

check_difficulty_data <- qc_checks %>%
  left_join(check_scores, "check_id") %>%
  left_join(check_positions, "check_id")

# construct the heatmap and highlights
plot_check_difficulty <- check_difficulty_data %>%
  ggplot() +
  geom_rect(aes(ymin=check_start, ymax=check_end, xmin=0, xmax=1, fill=score)) +
  ggrepel::geom_label_repel(
    aes(
      1,
      (check_end+check_start)/2,
      label=label_wrap(item, 35)
    ),
    nudge_x = 0.5,
    direction="y",
    data=check_difficulty_data %>%
      filter(score <= 0.5),
    size=4,
    lineheight=0.8,
    min.segment.length = 0,
    hjust=0
  ) +
  geom_segment(aes(x=0, xend=1, y=check_end, yend=check_end), data=check_positions, color="#444444", alpha=0.25) +
  geom_segment(aes(x=0, xend=1, y=aspect_end, yend=aspect_end), data=aspect_positions, color="#444444") +
  # hline_check + hline_aspect +
  # scale_fill_manual(values=set_names(qc_categories$color, qc_categories$category)) +
  # scale_fill_distiller("QC\ndifficulty", palette="Spectral", limits=c(0,1), breaks=c(0,0.5, 1), direction=1) +
  scale_fill_distiller("Average usability score", palette = "RdYlBu", limits=c(0,1), breaks=c(0,0.5, 1), guide=guide_colorbar(title.position = "bottom", title.hjust=0.5, barwidth = unit(2, "inches"))) +
  scale_x_continuous(NULL, breaks=NULL, limits=c(0, 12), expand=c(0,0)) +
  scale_y_reverse(NULL, breaks=NULL, expand=c(0,0)) +
  theme_pub() +
  theme(legend.position = "bottom", axis.line.y = element_blank())

plot_check_difficulty

##  ............................................................................
##  Create the overview figure                                              ####
plot_qc_overview <- wrap_plots(
  plot_spacer(),
  plot_tool_ordering,
  plot_spacer(),
  plot_category_labels,
  plot_individual_checks,
  plot_check_difficulty,
  ncol = 3,
  nrow = 2,
  heights = c(0.2, 0.8),
  widths = c(1, 8, 3)
)

plot_qc_overview

write_rds(
  plot_qc_overview,
  result_file("qc_overview.rds")
)
ggsave(
  result_file("qc_overview.pdf"),
  plot_qc_overview,
  width = 15,
  height = 18
)
