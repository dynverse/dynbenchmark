library(dynalysis)
library(tidyverse)
library(dynplot)

experiment("5-optimise_parameters/13-evaluate_with_real_datasets")

evals <- read_rds(derived_file("eval_outputs.rds"))
list2env(evals, environment())

# get ordering of methods
method_ord <- eval_overall %>%
  group_by(method_name) %>%
  summarise_if(is.numeric, mean) %>%
  arrange(desc(harm_mean)) %>%
  .$method_name

eval_overall <- eval_overall %>% mutate(method_name_f = factor(method_name, levels = rev(method_ord)))
eval_repl <- eval_repl %>% mutate(method_name_f = factor(method_name, levels = rev(method_ord)))
eval_ind <- eval_ind %>% mutate(method_name_f = factor(method_name, levels = rev(method_ord)))
eval_trajtype <- eval_trajtype %>% mutate(method_name_f = factor(method_name, levels = rev(method_ord)))
eval_trajtype_wa_wo <- eval_trajtype_wa_wo %>% mutate(method_name_f = factor(method_name, levels = rev(method_ord)))

prior_df <- eval_ind %>% select(method_name, prior_sr) %>% distinct() %>% mutate(prior_sr = prior_sr %>% str_replace_all("--required", ""))

# num_reals <- eval_ind %>% filter(method_short_name == "CTmaptpx", task_group == "real", param_group == "best", replicate == 1) %>%
#   group_by(trajectory_type) %>% summarise(n = n())
# num_synths <- eval_ind %>% filter(method_short_name == "CTmaptpx", task_group == "synthetic", param_group == "best", replicate == 1) %>%
#   group_by(trajectory_type) %>% summarise(n = n())
# write_tsv(num_reals, figure_file("trajtypes_real.tsv"))
# write_tsv(num_synths, figure_file("trajtypes_synth.tsv"))

zzz <- eval_trajtype_wa_wo %>% filter(task_group == "mean", trajectory_type_f == "overall")
yyy <- zzz %>%
  select(method_name, method_short_name, method_name_f, harm_mean, pct_errored_mean, rank_correlation_mean, rank_correlation_var, rank_rf_mse_mean, rank_rf_mse_var, rank_edge_flip_mean, rank_edge_flip_var, time_method_mean) %>%
  gather(metric, score, -method_name:-method_name_f) %>%
  mutate(metric_f = factor(metric, levels = c("harm_mean", "pct_errored_mean", "time_method_mean", "rank_correlation_mean", "rank_edge_flip_mean", "rank_rf_mse_mean", "rank_correlation_var", "rank_edge_flip_var", "rank_rf_mse_var")))

pdf(figure_file("1_overall_comparison.pdf"), 12, 8)
ggplot(yyy) +
  geom_bar(aes(method_name_f, score, fill = metric_f), stat = "identity") +
  facet_wrap(~metric_f, scales = "free", nrow = 3) +
  coord_flip() +
  theme_bw() +
  labs(x = NULL, y = NULL, fill = "Metric")

ggplot(yyy %>% left_join(prior_df, by = "method_name")) +
  geom_bar(aes(method_name_f, score, fill = prior_sr), stat = "identity") +
  facet_wrap(~metric_f, scales = "free", nrow = 3) +
  coord_flip() +
  theme_bw() +
  scale_fill_brewer(palette = "Dark2") +
  theme(legend.position = "bottom") +
  labs(x = NULL, y = NULL, fill = "Prior")
dev.off()

# plots
pdf(figure_file("2_trajtype_comparison.pdf"), 20, 8)
ggplot(eval_trajtype_wa_wo) +
  geom_point(aes(method_name_f, harm_mean)) +
  coord_flip() +
  theme_bw() +
  facet_grid(task_group~trajectory_type_f) +
  labs(
    x = NULL
  )

ggplot(eval_trajtype_wa_wo) +
  geom_point(aes(method_name_f, rank_correlation_mean)) +
  coord_flip() +
  theme_bw() +
  facet_grid(task_group~trajectory_type_f) +
  labs(
    x = NULL
  )


ggplot(eval_trajtype_wa_wo) +
  geom_point(aes(method_name_f, rank_edge_flip_mean)) +
  coord_flip() +
  theme_bw() +
  facet_grid(task_group~trajectory_type_f) +
  labs(
    x = NULL
  )

ggplot(eval_trajtype_wa_wo) +
  geom_point(aes(method_name_f, rank_rf_mse_mean)) +
  coord_flip() +
  theme_bw() +
  facet_grid(task_group~trajectory_type_f) +
  labs(
    x = NULL
  )

ggplot(eval_trajtype_wa_wo) +
  geom_point(aes(method_name_f, pct_errored_mean)) +
  coord_flip() +
  theme_bw() +
  facet_grid(task_group~trajectory_type_f) +
  labs(
    x = NULL
  )

ggplot(eval_trajtype_wa_wo, aes(harm_mean, rank_correlation_mean)) +
  geom_point() +
  ggrepel::geom_text_repel(aes(label = method_name)) +
  theme_bw() +
  facet_grid(task_group~trajectory_type_f)

ggplot(eval_trajtype_wa_wo, aes(harm_mean, rank_rf_mse_mean)) +
  geom_point() +
  ggrepel::geom_text_repel(aes(label = method_name)) +
  theme_bw() +
  facet_grid(task_group~trajectory_type_f)

ggplot(eval_trajtype_wa_wo, aes(harm_mean, rank_edge_flip_mean)) +
  geom_point() +
  ggrepel::geom_text_repel(aes(label = method_name)) +
  theme_bw() +
  facet_grid(task_group~trajectory_type_f)

ggplot(eval_trajtype_wa_wo, aes(harm_mean, pct_errored_mean)) +
  geom_point() +
  ggrepel::geom_text_repel(aes(label = method_name)) +
  theme_bw() +
  facet_grid(task_group~trajectory_type_f)

dev.off()





# error messages
error_messages_overall <-
  eval_ind %>%
  group_by(method_name, method_name_f) %>%
  mutate(num_datasets = n()) %>%
  ungroup() %>%
  filter(!sapply(error, is.null)) %>%
  rowwise() %>%
  mutate(
    error_message = error$message,
    error_message = str_replace_all(error_message, "/scratch/irc/personal/robrechtc/tmp//Rtmp[A-Za-z0-9_\\-/]*", "{tmpfile}")
  ) %>%
  ungroup() %>%
  group_by(method_name, method_name_f, error_message) %>%
  summarise(num = n(), pct = num / num_datasets[[1]], example_dataset = task_id[[1]]) %>%
  ungroup()
# error_reasons <- tribble(
#   ~partial_message, ~reason,
#   "reached elapsed time limit", "time limit",
#   "Cannot allocate memory", "memory limit",
#   "cannot open the connection", "error inside python code"
# )
# error_reason_fun <- function(error_message) {
#   greps <- sapply(error_reasons$partial_message, function(part_mess) {
#     grepl(part_mess, error_message)
#   })
#   apply(greps, 1, function(bools) {
#     if (any(bools)) {
#       error_reasons$reason[bools]
#     } else {
#       "error in method"
#     }
#   })
# }
#
# error_messages_overall <- error_messages_overall %>%
#   mutate(
#     error_reason = error_reason_fun(error_message),
#     error_reason_f = factor(error_reason, levels = names(sort(table(error_reason), decreasing = T)))
#   )

write_tsv(error_messages_overall, figure_file("error_reasons.tsv"))


pdf(figure_file("error_reasons.pdf"), 12, 6)
ggplot(error_messages_overall) +
  geom_bar(aes(method_name_f, pct, fill = error_reason_f), stat = "identity") +
  coord_flip() +
  scale_fill_brewer(palette = "Set1") +
  cowplot::theme_cowplot() +
  labs(x = NULL, y = "Percentage errored", fill = "Reason")
dev.off()


step_levels <- c("sessionsetup", "preprocessing", "method", "postprocessing", "wrapping", "sessioncleanup", "geodesic", "correlation",
                 "coranking", "mantel", "rf", "edge_flip")

time_ind <-
  eval_ind %>%
  select(method_name, method_name_f, task_id, pct_errored, error_message, trajectory_type_f, starts_with("time_")) %>%
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
  labs(x = NULL, fill = "Time step")
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
  labs(x = NULL, fill = "Time step")
ggsave(figure_file("timestep_permethod.pdf"), g, width = 10, height = 5)




### individual dataset plots
eval_ind <- eval_ind %>% mutate(harm_mean = apply(cbind(rank_correlation, rank_edge_flip, rank_rf_mse), 1, psych::harmonic.mean))
ggplot(eval_ind) +
  geom_point(aes(method_name_f, harm_mean, colour = trajectory_type_f)) +
  facet_wrap(~task_id) +
  coord_flip()


