library(dynalysis)
library(tidyverse)
library(dynplot)
library(glue)

# get paths
code <- "dyneval/0_testexecution_withtoy"
deriveddata_dir <- glue("analysis/data/derived_data/{code}")
figures_dir <- glue("analysis/figures/{code}")
results_dir <- glue("analysis/results/{code}")
dir.create(deriveddata_dir, recursive = T)
dir.create(figures_dir, recursive = T)
dir.create(results_dir, recursive = T)

# trying all methods
methods <- dyneval::get_descriptions(F)

# toys
tasks <- dyntoy::toy_tasks[5,]
task <- extract_row_to_list(tasks, 1)

# # synthetic dataset
# tasks <- readRDS(paste0("../dynalysis/analysis/data/derived_data/dyngen/tasks_v4.rds")) %>%
#   filter(
#     platform_id == "fluidigm_c1",
#     takesetting_type == "snapshot")
#
# tasks <- tasks[5,]

outs <- pbapply::pblapply(methods, function(method) {
  tryCatch({
    score <- execute_evaluation(tasks, method, parameters = list(), metrics = "auc_R_nx", timeout = 300)
    summary <- attr(score,"extras")$.summary
    prediction <- attr(score,"extras")$.models[[1]]
    attr(score,"extras") <- NULL
    meth_plot <- method$plot_fun(prediction)
    default_plot <- plot_default(prediction)
    strip_plot <- plot_strip(task, prediction)
    lst(method, score, summary, prediction, meth_plot, default_plot, strip_plot)
  }, error = function(e) NULL)
})
save(outs, file = glue("{results_dir}/outs.rds"))

check_df <- seq_along(outs) %>% map_df(function(i) {
  method <- methods[[i]]
  outm <- outs[[i]]
  data.frame(
    name = method$name,
    failed = length(outm) == 0 && is.null(outm),
    produced_ggplot = "ggplot" %in% class(outm$meth_plot),
    stringsAsFactors = F
  )
}) %>% as_data_frame()
check_df %>% as.data.frame

# rerun something
# i <- 16
# method <- methods[[i]]
# score <- execute_evaluation(tasks, method, parameters = list(), metrics = "auc_R_nx", timeout = 300)
# summary <- attr(score,"extras")$.summary
# prediction <- attr(score,"extras")$.models[[1]]
# attr(score,"extras") <- NULL
# meth_plot <- method$plot_fun(prediction)
# default_plot <- plot_default(prediction)
# outs[[i]] <- lst(method, score, summary, prediction, meth_plot, default_plot)
# rerun stop

plotlist <- outs[check_df$ggplot] %>% map(~ .$meth_plot)
num_plots <- length(plotlist)
ncol <- ceiling(sqrt(num_plots))
nrow <- ceiling(num_plots / ncol)

pdf(glue("{figures_dir}/0_theirplots.pdf"), ncol*8, nrow*8.4)
cowplot::plot_grid(plotlist = plotlist, ncol = ncol, align = "hv")
dev.off()

pdf(glue("{figures_dir}/1_multipleplots.pdf"), 24, 8.4)
for (i in seq_along(outs)) {
  if (check_df$ggplot[[i]]) {
    ou <- outs[[i]]
    ou_name <- ou$method$name
    g <- cowplot::plot_grid(
      ou$meth_plot,
      ou$default_plot,
      ou$strip_plot,
      nrow = 1
    )
    print(g)
  }
}
dev.off()

summary <- outs %>% map_df(~ .$summary)

# timings
timings <- summary %>%
  select(method_name, starts_with("time_")) %>%
  mutate(time_total = rowSums(.[,-1])) %>%
  arrange(time_total) %>%
  mutate(method_name_f = factor(method_name, levels = method_name)) %>%
  gather(part, time, -method_name, -method_name_f)

pdf(glue("{figures_dir}/2_timings.pdf"), 24, 16)
ggplot(timings) +
  geom_bar(aes(method_name_f, time, fill = part), stat = "identity") +
  cowplot::theme_cowplot() +
  coord_flip() +
  facet_wrap(~part, nrow = 2, scales = "free") +
  labs(y = "Time (s)", x = NULL, title = paste0("Timings on \"", task$id, "\", containing a ", nrow(task$counts), "-by-", ncol(task$counts), " expression matrix")) +
  scale_fill_brewer(palette = "Dark2")
dev.off()

# scores
scores <- summary %>%
  select(method_name, num_files_created, num_setseed_calls, mean_R_nx, auc_R_nx, Q_global, Q_local) %>%
  arrange(auc_R_nx) %>%
  mutate(method_name_f = factor(method_name, levels = method_name)) %>%
  gather(metric, score, -method_name, -method_name_f)

pdf(glue("{figures_dir}/3_scores.pdf"), 24, 16)
ggplot(scores) +
  geom_bar(aes(method_name_f, score, fill = metric), stat = "identity") +
  cowplot::theme_cowplot() +
  coord_flip() +
  facet_wrap(~metric, nrow = 2, scales = "free") +
  labs(x = "Score", y = NULL, title = paste0("Scores of a single execution on \"", task$id, "\", using the default parameters")) +
  scale_fill_brewer(palette = "Dark2")
dev.off()

