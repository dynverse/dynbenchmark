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
eval_ind <- eval_ind %>% mutate(method_name_f = factor(method_name, levels = rev(method_ord)))
eval_trajtype <- eval_trajtype %>% mutate(method_name_f = factor(method_name, levels = rev(method_ord)))
eval_trajtype_wa_wo <- eval_trajtype_wa_wo %>% mutate(method_name_f = factor(method_name, levels = rev(method_ord)))

# num_reals <- eval_ind %>% filter(method_short_name == "CTmaptpx", task_group == "real", param_group == "best", replicate == 1) %>%
#   group_by(trajectory_type) %>% summarise(n = n())
# num_synths <- eval_ind %>% filter(method_short_name == "CTmaptpx", task_group == "synthetic", param_group == "best", replicate == 1) %>%
#   group_by(trajectory_type) %>% summarise(n = n())
# write_tsv(num_reals, figure_file("trajtypes_real.tsv"))
# write_tsv(num_synths, figure_file("trajtypes_synth.tsv"))


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
  geom_point(aes(method_name_f, rank_correlation)) +
  coord_flip() +
  theme_bw() +
  facet_grid(task_group~trajectory_type_f) +
  labs(
    x = NULL
  )

ggplot(eval_trajtype_wa_wo) +
  geom_point(aes(method_name_f, rank_edge_flip)) +
  coord_flip() +
  theme_bw() +
  facet_grid(task_group~trajectory_type_f) +
  labs(
    x = NULL
  )

ggplot(eval_trajtype_wa_wo) +
  geom_point(aes(method_name_f, rank_rf_mse)) +
  coord_flip() +
  theme_bw() +
  facet_grid(task_group~trajectory_type_f) +
  labs(
    x = NULL
  )

ggplot(eval_trajtype_wa_wo) +
  geom_point(aes(method_name_f, pct_errored)) +
  coord_flip() +
  theme_bw() +
  facet_grid(task_group~trajectory_type_f) +
  labs(
    x = NULL
  )

ggplot(eval_trajtype_wa_wo, aes(harm_mean, rank_correlation)) +
  geom_point() +
  ggrepel::geom_text_repel(aes(label = method_name)) +
  theme_bw() +
  facet_grid(task_group~trajectory_type_f)

ggplot(eval_trajtype_wa_wo, aes(harm_mean, rank_rf_mse)) +
  geom_point() +
  ggrepel::geom_text_repel(aes(label = method_name)) +
  theme_bw() +
  facet_grid(task_group~trajectory_type_f)

ggplot(eval_trajtype_wa_wo, aes(harm_mean, rank_edge_flip)) +
  geom_point() +
  ggrepel::geom_text_repel(aes(label = method_name)) +
  theme_bw() +
  facet_grid(task_group~trajectory_type_f)

ggplot(eval_trajtype_wa_wo, aes(harm_mean, pct_errored)) +
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
  mutate(error_message = error$message) %>%
  ungroup() %>%
  group_by(method_name, method_name_f, error_message) %>%
  summarise(num = n(), pct = num / num_datasets[[1]]) %>%
  ungroup()
error_reasons <- tribble(
  ~partial_message, ~reason,
  "reached elapsed time limit", "time limit",
  "Cannot allocate memory", "memory limit",
  "cannot open the connection", "error inside python code"
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


step_levels <- c("sessionsetup", "preprocessing", "method", "postprocessing", "wrapping", "sessioncleanup", "geodesic", "correlation",
                 "coranking", "mantel", "rf", "edge_flip")

time_ind <-
  eval_ind %>%
  select(method_name, method_name_f, task_id, pct_errored, error_message, trajectory_type_f, starts_with("time_")) %>%
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


