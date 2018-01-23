library(dynalysis)
library(tidyverse)
library(dynplot)

experiment("5-optimise_parameters/11-evaluate_with_real_datasets-CORRELATION")

source("analysis/analyses/4-method_characterization/0_common.R")

evals <- read_rds(derived_file("eval_outputs.rds", "5-optimise_parameters/11-evaluate_with_real_datasets-CORRELATION"))
list2env(evals, environment())
method_char <- read_rds(derived_file("method_df_evaluated.rds", "method_characteristics"))
method_df_evaluated <- method_char %>%
  rowwise() %>%
  do({
    df <- .
    method_short_name <- strsplit(df$ids, ",")[[1]]
    data.frame(df, method_short_name, stringsAsFactors = FALSE, check.names = FALSE)
  })

eval_ind <- eval_ind %>% rowwise() %>% mutate(error_message = ifelse(is.null(error), "", error$message)) %>% ungroup()

num_reals <- eval_ind %>% filter(method_short_name == "CTmaptpx", task_group == "real", param_group == "best", replicate == 1) %>%
  group_by(trajectory_type) %>% summarise(n = n())
num_synths <- eval_ind %>% filter(method_short_name == "CTmaptpx", task_group == "synthetic", param_group == "best", replicate == 1) %>%
  group_by(trajectory_type) %>% summarise(n = n())
write_tsv(num_reals, figure_file("trajtypes_real.tsv"))
write_tsv(num_synths, figure_file("trajtypes_synth.tsv"))

# adding totals and means to table
eval_trajtype_wm <- eval_trajtype %>%
  group_by(method_name, method_name_f, method_short_name, param_group, trajectory_type, trajectory_type_f) %>%
  summarise_if(is.numeric, mean) %>%
  ungroup() %>%
  mutate(task_group = "mean") %>%
  bind_rows(eval_trajtype)

eval_overall_wm <- eval_overall %>%
  group_by(method_name, method_name_f, method_short_name, param_group) %>%
  summarise_if(is.numeric, mean) %>%
  ungroup() %>%
  mutate(task_group = "mean") %>%
  bind_rows(eval_overall)

eval_trajtype_wa_wo <- eval_overall_wm %>%
  mutate(trajectory_type = "overall") %>%
  bind_rows(eval_trajtype_wm) %>%
  mutate(trajectory_type_f = factor(trajectory_type, levels = c("overall", trajtype_ord)))

# plots
pdf(figure_file("2_trajtype_comparison.pdf"), 20, 8)
ggplot(eval_trajtype_wa_wo) +
  geom_point(aes(method_name_f, rank_correlation)) +
  coord_flip() +
  theme_bw() +
  facet_grid(task_group~trajectory_type_f) +
  labs(
    x = NULL,
    colour = "Parameter\ngroup",
    y = "Mean quantile score of correlation in pairwise tented geodesic distance between cells",
    title = "Evaluation of trajectory inference methods with default parameters"
  )

# ggplot(eval_trajtype_wa_wo) +
#   geom_point(aes(method_name_f, rank_mrsq)) +
#   coord_flip() +
#   theme_bw() +
#   facet_grid(task_group~trajectory_type_f) +
#   labs(
#     x = NULL,
#     colour = "Parameter\ngroup",
#     y = "Mean quantile score of OOB-MSE score when predicting gold milestone percentages",
#     title = "Evaluation of trajectory inference methods with default parameters"
#   )
# ggplot(eval_trajtype_wa_wo) +
#   geom_point(aes(method_name_f, rank_mmse)) +
#   coord_flip() +
#   theme_bw() +
#   facet_grid(task_group~trajectory_type_f) +
#   labs(
#     x = NULL,
#     colour = "Parameter\ngroup",
#     y = "Mean quantile score of OOB-MSE score when predicting gold milestone percentages",
#     title = "Evaluation of trajectory inference methods with default parameters"
#   )

ggplot(eval_trajtype_wa_wo) +
  geom_point(aes(method_name_f, percentage_errored)) +
  coord_flip() +
  theme_bw() +
  facet_grid(task_group~trajectory_type_f) +
  labs(
    x = NULL,
    colour = "Parameter\ngroup",
    y = "Percentage errored",
    title = "Error rates of trajectory inference methods with default parameters"
  )
dev.off()




# error messages
error_messages_overall <-
  eval_ind %>%
  group_by(method_name, method_name_f) %>%
  mutate(num_datasets = n()) %>%
  ungroup() %>%
  filter(!sapply(error, is.null)) %>%
  rowwise() %>%
  mutate(error_message = error$message) %>%
  ungroup() %>%
  group_by(method_name, method_name_f, error_message) %>%
  summarise(num = n(), pct = num / num_datasets[[1]]) %>%
  ungroup()
error_reasons <- tribble(
  ~partial_message, ~reason,
  "reached elapsed time limit", "time limit",
  "Cannot allocate memory", "memory limit",
  "cannot open the connection", "error inside python code",
  "Column `cell_id` must be a 1d atomic vector or a list", "bug in wrapper"
)
error_reason_fun <- function(error_message) {
  greps <- sapply(error_reasons$partial_message, function(part_mess) {
    grepl(part_mess, error_message)
  })
  apply(greps, 1, function(bools) {
    if (any(bools)) {
      error_reasons$reason[bools]
    } else {
      "error in method"
    }
  })
}

error_messages_overall <- error_messages_overall %>%
  mutate(
    error_reason = error_reason_fun(error_message),
    error_reason_f = factor(error_reason, levels = names(sort(table(error_reason), decreasing = T)))
  )


pdf(figure_file("error_reasons.pdf"), 12, 6)
ggplot(error_messages_overall) +
  geom_bar(aes(method_name_f, pct, fill = error_reason_f), stat = "identity") +
  coord_flip() +
  scale_fill_brewer(palette = "Set1") +
  cowplot::theme_cowplot() +
  labs(x = NULL, y = "Percentage errored", fill = "Reason")
dev.off()

tasks <- read_rds(derived_file("tasks.rds"))
tasks_dimensionality <- tasks %>% mutate(num_cells = sapply(cell_ids, length), num_genes = sapply(feature_info, nrow)) %>% select(id, num_cells, num_genes)

timeout_rates <- eval_ind %>%
  group_by(task_id, task_group) %>%
  summarise(mean_timeout = mean(grepl("time limit", error_message))) %>%
  arrange(desc(mean_timeout)) %>%
  ungroup() %>%
  left_join(tasks_dimensionality, by = c("task_id"="id"))

g1 <- ggplot(timeout_rates) + geom_point(aes(num_cells, mean_timeout, colour = task_group)) + cowplot::theme_cowplot()
g2 <- ggplot(timeout_rates) + geom_point(aes(num_genes, mean_timeout, colour = task_group)) + cowplot::theme_cowplot()
g3 <- ggplot(timeout_rates) + geom_point(aes(sqrt(num_genes*num_cells), mean_timeout, colour = task_group)) + cowplot::theme_cowplot()

pdf(figure_file("mean_timeout.pdf"), 20, 5)
cowplot::plot_grid(g1, g2, g3, nrow = 1)
dev.off()

step_levels <- c("sessionsetup", "preprocessing", "method", "postprocessing", "wrapping", "sessioncleanup", "geodesic", "correlation",
                 "coranking", "mantel")

time_ind <-
  eval_ind %>%
  select(method_name, method_name_f, task_id, percentage_errored, error_message, trajectory_type_f, starts_with("time_")) %>%
  mutate(error_reason = error_reason_fun(error_message)) %>%
  gather(step, time, starts_with("time")) %>%
  mutate(
    step = gsub("time_", "", step),
    step_f = factor(step, levels = step_levels)
  )

task_ordering <- time_ind %>%
  group_by(task_id) %>%
  summarise(time = sum(time, na.rm = T)) %>%
  arrange(desc(time))

method_ordering <- time_ind %>%
  group_by(method_name, step_f) %>%
  summarise(time = mean(time, na.rm = T)) %>%
  summarise(time = sum(time, na.rm=T)) %>%
  arrange(desc(time))

time_ind <- time_ind %>% mutate(
  task_id_f = factor(task_id, levels = task_ordering$task_id),
  method_name_f = factor(method_name, levels = method_ordering$method_name)
)

ggplot(time_ind) +
  geom_bar(aes(task_id_f, time, fill = step_f), stat = "identity") +
  facet_wrap(~method_name, scales = "free_y") +
  scale_fill_brewer(palette = "Set3")


g <- time_ind %>%
  group_by(trajectory_type_f, method_name, step_f) %>%
  summarise(time = mean(time, na.rm = T)) %>%
  ungroup() %>%
  ggplot() +
  geom_bar(aes(trajectory_type_f, time, fill = step_f), stat = "identity", position = position_stack(reverse = TRUE)) +
  facet_wrap(~method_name, scales = "free") +
  scale_fill_brewer(palette = "Set3") +
  cowplot::theme_cowplot() +
  coord_flip() +
  labs(x = NULL)
ggsave(figure_file("timestep_permethodandtrajtype.pdf"), g, width = 20, height = 8)

g <- time_ind %>%
  group_by(method_name_f, step_f) %>%
  summarise(time = mean(time, na.rm = T)) %>%
  ungroup() %>%
  ggplot() +
  geom_bar(aes(method_name_f, time, fill = step_f), stat = "identity", position = position_stack(reverse = TRUE)) +
  scale_fill_brewer(palette = "Set3") +
  cowplot::theme_cowplot() +
  coord_flip() +
  labs(x = NULL)
ggsave(figure_file("timestep_permethod.pdf"), g, width = 10, height = 5)
