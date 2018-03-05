library(cowplot)
library(tidyverse)
library(dynalysis)

scores <- c("harm_mean", "rank_correlation", "rank_edge_flip", "rank_rf_mse")

methods <- read_rds(derived_file("methods.rds", experiment_id = "4-method_characterisation"))
outputs_list <- read_rds(derived_file("outputs_postprocessed.rds", "5-optimise_parameters/3-evaluate_parameters"))
method_scores <- outputs_list$outputs_summmethod_totals %>%
  rename(method_id = method_short_name) %>%
  filter(method_id %in% methods$method_id) %>%
  filter(task_source=="mean")
indrep_scores <- outputs_list$outputs_summrepl %>%
  rename(method_id = method_short_name) %>%
  filter(method_id %in% methods$method_id)
method_order <- method_scores %>%
  arrange(-harm_mean) %>%
  filter(method_id %in% methods$method_id) %>%
  pull(method_id)




##  ............................................................................
##  Dataset variability                                                     ####

performance_variability_tests <- map(method_order[2:4], function(to) {
  test <- wilcox.test(indrep_scores$harm_mean[indrep_scores$method_id == method_order[1]], indrep_scores$harm_mean[indrep_scores$method_id == to], paired = T)
  tibble(
    to = to,
    p_value = test$p.value
  )
}) %>% bind_rows() %>%
  mutate(q_value = p.adjust(p_value, "BH")) %>%
  mutate(i = rev(row_number()))


ymax <- max(indrep_scores$harm_mean)
indrep_scores %>%
  mutate(method_id = factor(method_id, method_order)) %>%
  ggplot(aes(method_id, harm_mean)) +
    geom_boxplot() +
    geom_segment(aes(x = method_order[[1]], xend = to, y=ymax+i/10, yend=ymax+i/10), data=performance_variability_tests) +
    geom_text(aes(x = rev(i)/2+1, y = ymax+i/10, label=label_pvalue(p_value)), data=performance_variability_tests, vjust=0) +
    coord_flip()

performance_variability_tests <- combn(method_order, 2) %>% t %>% as_data_frame() %>% magrittr::set_colnames(c("from", "to")) %>%
  mutate(test = map2(from, to, function(from, to) {
    wilcox.test(indrep_scores$harm_mean[indrep_scores$method_id == to], indrep_scores$harm_mean[indrep_scores$method_id == from], paired = T, conf.int=T, alternative="less")
  })) %>%
  mutate(
    p_value = map_dbl(test, "p.value"),
    q_value = p.adjust(p_value, "BH"),
    log_q_value = log10(q_value),
    estimate = map_dbl(test, "estimate"),
    effect = ifelse(estimate > 0,  "↗", "↘")
  ) %>%
  mutate(
    from = factor(from, method_order),
    to = factor(to, method_order)
  )
performance_variability_tests %>%
  ggplot() +
  geom_tile(aes(to, from, fill=q_value)) +
  scale_fill_gradientn(colors=RColorBrewer::brewer.pal(6, "YlGnBu"), values=c(0, 0.0001, 0.001, 0.01, 0.1, 1))



first_significant <- performance_variability_tests %>%
  group_by(from) %>%
  filter(q_value < 0.05) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  mutate(ystart = as.numeric(from), yend = as.numeric(to)) %>%
  mutate(x=0)

for (i in seq_len(nrow(first_significant))) {
  row <- extract_row_to_list(first_significant, i)
  overlapping <- first_significant$ystart <= row$yend & row$ystart <= first_significant$yend
  x <- which(!((1:1000 %in% first_significant$x[overlapping]))) %>% first()
  first_significant$x[i] <- x
}

first_significant %>%
  ggplot() +
    geom_segment(aes(x=x, xend=x, y=ystart, yend=yend), alpha=0.5) +
    geom_segment(aes(x=x-0.5, xend=x, y=ystart, yend=ystart), alpha=0.5) +
    geom_segment(aes(x=x-0.5, xend=x, y=yend, yend=yend), alpha=0.5) +
    scale_y_continuous(breaks=seq_along(method_order), label=method_order)

