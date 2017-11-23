library(dynalysis)
library(tidyverse)
library(dynplot)

experiment("dyneval/0_testexecution_local_withtoy")

# trying all methods
methods <- dyneval::get_descriptions(F)

# toys
tasks <- dyntoy::toy_tasks[5,]
task <- extract_row_to_list(tasks, 1)

metrics <- c("auc_R_nx", "correlation")
parameters <- list()
timeout <- 600

# run each method
# outs <- pbapply::pblapply(seq_along(methods), function(mi) {
#   tryCatch({
#     method <- methods[[mi]]
#     score <- execute_evaluation(tasks, method, parameters = parameters, metrics = metrics, timeout = timeout)
#     summary <- attr(score,"extras")$.summary
#     prediction <- attr(score,"extras")$.models[[1]]
#     attr(score,"extras") <- NULL
#     lst(method, score, summary, prediction)
#   }, error = function(e) NULL)
# })
# saveRDS(outs, file = result_file("outs.rds"))
outs <- readRDS(file = result_file("outs.rds"))

plots <- pbapply::pblapply(seq_along(outs), cl = 8, function(i) {
  method <- methods[[i]]
  prediction <- outs[[i]]$prediction
  if (!is.null(prediction)) {
    meth_plot <- method$plot_fun(prediction)
    default_plot <- dyneval::plot_default(prediction)
    strip_plot <- dynplot::plot_strip_connections(prediction, task)
    lst(meth_plot, default_plot, strip_plot)
  } else {
    NULL
  }
})

check_df <- seq_along(outs) %>% map_df(function(i) {
  method <- methods[[i]]
  outm <- outs[[i]]
  plotm <- plots[[i]]
  data.frame(
    name = method$name,
    failed = (length(outm) == 0 && is.null(outm)) || (length(outm$prediction) == 0 && is.null(outm$prediction)),
    produced_ggplot = "ggplot" %in% class(plotm$meth_plot),
    error = ifelse(is.null(outm$summary$error[[1]]), "", outm$summary$error[[1]][[1]]),
    stringsAsFactors = F
  )
}) %>% as_data_frame()
check_df %>% as.data.frame

# rerun something
# i <- 27
# method <- methods[[i]]
# score <- execute_evaluation(tasks, method, parameters = parameters, metrics = metrics, timeout = timeout)
# summary <- attr(score,"extras")$.summary
# prediction <- attr(score,"extras")$.models[[1]]
# attr(score,"extras") <- NULL
# outs[[i]] <- lst(method, score, summary, prediction)
# rerun end

plotlist <- plots[check_df$produced_ggplot] %>% map(~ .$meth_plot)
num_plots <- length(plotlist)
ncol <- ceiling(sqrt(num_plots))
nrow <- ceiling(num_plots / ncol)

pdf(figure_file("0_theirplots.pdf"), ncol*8, nrow*8.4)
cowplot::plot_grid(plotlist = plotlist, ncol = ncol, align = "hv")
dev.off()

pdf(figure_file("1_multipleplots.pdf"), 24, 8.4)
for (i in seq_along(outs)) {
  if (check_df$produced_ggplot[[i]]) {
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

pdf(figure_file("2_timings.pdf"), 24, 16)
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
  select(method_name, correlation, mean_R_nx, auc_R_nx, Q_global, Q_local, num_files_created, num_setseed_calls) %>%
  arrange(auc_R_nx) %>%
  mutate(method_name_f = factor(method_name, levels = method_name)) %>%
  gather(metric, score, -method_name, -method_name_f)

pdf(figure_file("3_scores.pdf"), 24, 16)
ggplot(scores) +
  geom_bar(aes(method_name_f, score, fill = metric), stat = "identity") +
  cowplot::theme_cowplot() +
  coord_flip() +
  facet_wrap(~metric, nrow = 2, scales = "free") +
  labs(x = "Score", y = NULL, title = paste0("Scores of a single execution on \"", task$id, "\", using the default parameters")) +
  scale_fill_brewer(palette = "Dark2")
dev.off()

