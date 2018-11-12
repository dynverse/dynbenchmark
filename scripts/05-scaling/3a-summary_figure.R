##########################################################
###############        CLASSIFY TIME       ###############
##########################################################

classification_design <- bind_rows(
  tibble(
    feature = "n_cells",
    n_cells = seq(100, 10000, 100),
    n_features = 10000
  ),
  tibble(
    feature = "n_features",
    n_features = seq(100, 10000, 100),
    n_cells = 10000
  )
)

method_predict_time <- models_nocontrol %>% select(method_id, predict_time) %>% deframe()
method_predict_mem <- models_nocontrol %>% select(method_id, predict_mem) %>% deframe()

classification_values <- crossing(
  classification_design,
  method_id = names(method_predict_time)
) %>%
  group_by(method_id) %>%
  mutate(
    time = first(method_predict_time[method_id])(n_cells, n_features),
    mem = first(method_predict_mem[method_id])(n_cells, n_features)
  ) %>%
  ungroup()

# original solution
#
# classify_curve <- function(slope, curvature) {
#   case_when(
#     slope < -.001 ~ "negative slope",
#     slope < 0.001 ~ "<linear",
#     curvature < -0.05 ~ "<linear",
#     curvature < 0.05 ~ "linear",
#     TRUE ~ ">linear"
#   )
# }
#
# time_classification <- time_classification_timings %>%
#   group_by(method_id, feature) %>%
#   summarise(
#     slope = mean(diff(time, differences = 1)),
#     curvature = mean(seq(0, 1, length.out = n()) - scale_minmax(time)),
#     class = classify_curve(slope, curvature)
#   ) %>%
#   ungroup()


classify_curve <- function(method_id, feature, value, n_cells, n_features) {
  # detect low or negative slopes
  # these are likely mistakes in the model
  slope <- mean(diff(value))
  if (slope < -.001) return("negative slope")
  if (slope < .001) return("<linear")

  # detect sublinear separately
  curvature <- mean(seq(0, 1, length.out = length(value)) - scale_minmax(value))
  if (curvature < -0.1) return("<linear")

  dat <-
    data_frame(y = value, n_cells, n_features) %>%
    select(x = !!feature, y)
  dat <- dat %>%
    mutate(
      x = x / max(x),
      y = y / max(y)
    )

  # train model and obtain coefs
  # using glmnet instead of lm because lm would easily overfit
  datm <- stats::model.matrix(y ~ log(x) + sqrt(x) + x + I(x^2) + I(x^3), data = dat)[,-1]
  rr.cv <- glmnet::cv.glmnet(datm, dat$y, alpha = 1)
  rr.fit <- glmnet::glmnet(datm, dat$y, alpha = 1, lambda = rr.cv$lambda.min)
  coeffs <- rr.fit$beta[,1]

  max_term <-
    if (any(coeffs > .25)) {
      # if at least one coefficient is very high,
      # take the most complex one
      which(coeffs > .25) %>% tail(1) %>% names()
    } else {
      # otherwise, just take the largest coefficient
      which.max(coeffs) %>% names()
    }

  c(
    "log(x)" = "<linear",
    "sqrt(x)" = "<linear",
    "x" = "linear",
    "I(x * log(x))" = "nlogn",
    "I(x^2)" = "quadratic",
    "I(x^3)" = ">quadratic",
    "I(x^4)" = ">quadratic",
    "exp(x)" = "exponential"
  )[max_term] %>% unname()
}



# biologists solution
#
# classify_curve <- function(method_id, feature, time, n_cells, n_features) {
#   test <- 1:5
#   mean_diffs <- map_dbl(test, ~ mean(diff(time, differences = .)))
#   num <- first(which(mean_diffs < .1))
#   if (is.na(num)) {
#     "exponential"
#   } else if (num == 1) {
#     "<linear"
#   } else if (num == 2) {
#     "linear"
#   } else if (num == 3) {
#     "quadratic"
#   } else if (num < max(test)) {
#     ">quadratic"
#   } else {
#     "exponential"
#   }
# }

classifications <-
  classification_values %>%
  group_by(method_id, feature) %>%
  summarise(
    slope_time = mean(diff(time, differences = 1)),
    curvature_time = mean(seq(0, 1, length.out = n()) - scale_minmax(time)),
    class_time = classify_curve(method_id[[1]], feature[[1]], time, n_cells, n_features),
    slope_mem = mean(diff(mem, differences = 1)),
    curvature_mem = mean(seq(0, 1, length.out = n()) - scale_minmax(mem)),
    class_mem = classify_curve(method_id[[1]], feature[[1]], mem, n_cells, n_features)
  ) %>%
  ungroup()

g <- classifications %>% crossing(pred = c("time", "mem")) %>% mutate(class = ifelse(pred == "time", class_time, class_mem), curvature = ifelse(pred == "time", curvature_time, curvature_mem)) %>%
  ggplot() + geom_density(aes(curvature, colour = class)) + facet_wrap(~feature + pred, ncol = 2) + theme_bw() + scale_x_continuous(breaks = seq(-.5, .5, by = .05))
ggsave(result_file("class_versus_curvature.pdf"), g, width = 12, height = 8)

##########################################################
###############  GENERATE SUMMARY FIGURE   ###############
##########################################################

method_order <- models_nocontrol %>% arrange(-time_lpred) %>% pull(method_id)
scale_y_methods <- scale_y_continuous("", breaks = seq_along(method_order), labels = label_method(method_order), expand = c(0, 0))
scale_y_methods_empty <- scale_y_continuous("", breaks = seq_along(method_order) + 0.5, labels = NULL, expand = c(0, 0))
no_margin_sides <- theme(plot.margin = margin(), axis.ticks.y = element_blank(), axis.title.y = element_blank())

time_limits <- c(log10(0.1), log10(60*60*72.01))
time_breaks <- log10(c(1, 60, 60*60, 60*60*24.01))
scale_x_time <- scale_x_continuous(label_long("average_time"), limits = time_limits, breaks = time_breaks, labels = label_time(10^time_breaks), expand = c(0, 00), position = "top")
scale_x_time
even_background <- geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = as.numeric(method_id) - 0.5, ymax = as.numeric(method_id) + 0.5), data = tibble(method_id = factor(method_order, method_order)) %>% filter(as.numeric(method_id) %% 2 == 1), fill = "#DDDDDD")

## Plot average time
plot_average_time <-
  models_nocontrol %>%
  mutate(
    method_id = factor(method_id, method_order),
    time_lpred = ifelse(time_lpred < -1, -0.8, time_lpred)
  ) %>%
  select(method_id, time_lpred) %>%
  ggplot() +
  even_background +
  geom_rect(aes(xmin = time_limits[[1]], xmax = time_lpred, ymin = as.numeric(method_id) - 0.4, ymax = as.numeric(method_id) + 0.4), stat = "identity", fill = "#333333") +
  geom_text(aes(x = time_lpred, y = as.numeric(method_id), label = paste0(" ", label_time(10^time_lpred))), hjust = 0) +
  scale_y_methods +
  scale_x_time +
  theme_pub() +
  theme(panel.grid.minor.y = element_line(color = "#AAAAAA"), plot.margin = margin())
plot_average_time

## Plot average memory
mem_limits <- log10(c(10^8, 10^10))
mem_breaks <- log10(c(10^8, 10^9, 10^10))
scale_x_mem <- scale_x_continuous(label_long("average_max_memory"), limits = mem_limits, breaks = mem_breaks, labels = label_memory(10^mem_breaks), expand = c(0, 0), position = "top")

plot_average_memory <-
  models_nocontrol %>%
  mutate(method_id = factor(method_id, method_order)) %>%
  select(method_id, mem_lpred) %>%
  ggplot() +
  even_background +
  geom_rect(aes(xmin = mem_limits[[1]], xmax = mem_lpred, ymin = as.numeric(method_id) - 0.4, ymax = as.numeric(method_id) + 0.4), stat = "identity", fill = "#333333") +
  geom_text(aes(x = mem_lpred, y = as.numeric(method_id), label = paste0(" ", label_memory(10^mem_lpred))), hjust = 0) +
  scale_y_methods +
  scale_x_mem +
  theme_pub() +
  theme(panel.grid.minor.y = element_line(color = "#AAAAAA"), plot.margin = margin())

plot_average_memory

## Plot the classification and small multiples
plotdata_classification <-
  classification_values %>%
  left_join(classifications, c("method_id", "feature")) %>%
  mutate(method_id = factor(method_id, method_order)) %>%
  mutate(
    ymin = as.numeric(method_id),
    xmin = as.numeric(factor(feature))
  ) %>%
  group_by(
    method_id, feature
  ) %>%
  filter(
    row_number() %% 10 == 0
  ) %>%
  mutate(
    x = xmin + row_number() / n() * 0.95 + 0.025,
    y_time = ymin + scale_minmax(time) * 0.9 + 0.05,
    y_mem = ymin + scale_minmax(mem) * 0.9 + 0.05
  ) %>%
  ungroup()

plotdata_classification_boxes <-
  plotdata_classification %>%
  group_by(
    method_id, feature
  ) %>%
  summarise(
    xmin = first(xmin),
    xmax = xmin + 1,
    ymin = first(ymin),
    ymax = ymin + 1
  ) %>%
  left_join(classifications, c("method_id", "feature"))

plotdata_classification <- plotdata_classification %>% select(method_id, feature, x, y_time, y_mem, class_time, class_mem)

# make plot
class_palette <- c("negative slope" = "darkgray", "<linear" = "#3d87a6", "linear" = "#9bcde1", ">linear" = "#d73027", "quadratic" = "#d78a27", ">quadratic" = "#d73027", "exponential" = "black")
white <- c(">quadratic", "<linear", "quadratic", ">linear")


plot_time_classification <-
  ggplot(plotdata_classification, aes(group = paste0(method_id, feature))) +
  geom_rect(
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = class_time),
    data = plotdata_classification_boxes %>% mutate(class_time = factor(class_time, levels = names(class_palette))),
    color = NA
  ) +
  geom_line(aes(x = x, y = y_time), plotdata_classification %>% filter(!class_time %in% white), color = "#333333", size = .75) +
  geom_line(aes(x = x, y = y_time), plotdata_classification %>% filter(class_time %in% white), color = "white", size = 1) +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), data = plotdata_classification_boxes, color = "#333333", fill = NA, size = 0.25) +
  scale_y_methods_empty +
  scale_x_continuous("Time complexity" , expand = c(0, 0), breaks = c(1.5, 2.5), labels = c("Cells", "Features"), position = "top") +
  scale_fill_manual("", values = class_palette) +
  theme_pub() +
  theme(legend.position = "bottom") +
  no_margin_sides

plot_time_classification

plot_mem_classification <-
  ggplot(plotdata_classification, aes(group = paste0(method_id, feature))) +
  geom_rect(
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = class_mem),
    data = plotdata_classification_boxes %>% mutate(class_mem = factor(class_mem, levels = names(class_palette))),
    color = NA
  ) +
  geom_line(aes(x = x, y = y_mem), plotdata_classification %>% filter(!class_mem %in% white), color = "#333333", size = .75) +
  geom_line(aes(x = x, y = y_mem), plotdata_classification %>% filter(class_mem %in% white), color = "white", size = 1) +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), data = plotdata_classification_boxes, color = "#333333", fill = NA, size = 0.25) +
  scale_y_methods_empty +
  scale_x_continuous("Memory complexity" , expand = c(0, 0), breaks = c(1.5, 2.5), labels = c("Cells", "Features"), position = "top") +
  scale_fill_manual("", values = class_palette) +
  theme_pub() +
  theme(legend.position = "bottom") +
  no_margin_sides

plot_mem_classification

##
slope_limits <- c(-2, log10(60*60*72.01))
slope_breaks <- log10(c(1, 60, 60*60, 60*60*24.01))
scale_x_slope <- scale_x_continuous(limits = slope_limits, breaks = slope_breaks, labels = label_time(10^slope_breaks), expand = c(0, 0), position = "top")

plot_slope <- function(feature) {
  classifications %>%
    filter(feature == !!feature) %>%
    mutate(method_id = factor(method_id, method_order)) %>%
    mutate(slope_time = scales::squish(slope_time, 10^slope_limits)) %>%
    ggplot() +
    even_background +
    geom_rect(aes(xmin = first(slope_limits), xmax = log10(slope_time), ymin = as.numeric(method_id) - 0.4, ymax = as.numeric(method_id) + 0.4), stat = "identity", fill = "#333333") +
    geom_text(aes(x = log10(slope_time), y = as.numeric(method_id), label = paste0(" ", label_time(slope_time))), hjust = 0) +
    scale_y_methods_empty +
    scale_x_slope +
    labs(x = label_long(paste0("time_per_100_", gsub("n_", "", feature)))) +
    theme_pub() +
    no_margin_sides +
    theme(panel.grid.major.y = element_line(color = "#AAAAAA"))
}

plot_slope_cells <- plot_slope("n_cells")
plot_slope_features <- plot_slope("n_features")
plot_slope_cells


##

g <- patchwork::wrap_plots(
  plot_average_time,
  plot_time_classification,
  # plot_slope_cells,
  # plot_slope_features,
  plot_average_memory,
  plot_mem_classification,
  # widths = c(2, 1, 2, 2, 2),
  widths = c(2, 1, 2, 1),
  nrow = 1
)

g
ggsave(result_file("ranking.pdf"), g, width = 12, height = 12)
write_rds(g, derived_file("ranking.rds"))
