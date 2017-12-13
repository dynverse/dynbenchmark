library(dynalysis)
library(tidyverse)
library(dynplot)

experiment("5-optimise_parameters/8-evaluate_with_real_datasets")

dataset_names <- list_datasets()
tasks <- pbapply::pblapply(dataset_names, load_dataset) %>% list_as_tibble() %>%
  mutate(nrow = map_int(expression, nrow), ncol = map_int(expression, ncol))

tasks <- tasks %>% filter(nrow < 2000)

best_parm <- read_rds(result_file("best_params.rds", "5-optimise_parameters/7-train_parameters_with_synthetic_datasets")) %>%
  mutate(train_score = pmax(0, train_score), test_score = pmax(0, test_score))
selected_parms <- best_parm  %>% group_by(method_name, fold_i) %>% mutate(norm_score = test_score / mean(test_score)) %>%
  ungroup() %>% group_by(method_name) %>% arrange(desc(norm_score)) %>% slice(1) %>% ungroup() %>% arrange(method_name)

methods <- get_descriptions(as_tibble = F)
metrics <- "auc_R_nx"
timeout <- 300

method_names <- unique(selected_parms$method_name)

for (mn in method_names) {
  output_file <- derived_file(paste0(mn, ".rds"))

  if (!file.exists(output_file)) {
    cat("Running ", mn, "\n", sep = "")
    method <- methods[[mn]]
    parameters <- selected_parms %>% filter(method_name == mn) %>% dynutils::extract_row_to_list(1) %>% .$params
    parameters <- lapply(names(parameters), function(prnm) {
      parset_par <- method$par_set$pars[[prnm]]
      parv <- parameters[[prnm]]
      if (!is.null(parset_par$trafo)) {
        parv <- parset_par$trafo(parv)
      }
      parv
    }) %>% setNames(names(parameters))

    score <- execute_evaluation(
      tasks = tasks,
      method = method,
      parameters = parameters,
      metrics = metrics,
      timeout = timeout,
      output_model = T,
      error_score = 0
    )

    extras <- attr(score, "extras")
    models <- extras$.models
    summary <- extras$.summary
    attr(score, "extras") <- NULL
    write_rds(lst(method_name = mn, parameters, score, models, summary), output_file)
  }
}

# reprocess files
# for (mn in method_names) {
#   cat("Reprocessing ", mn, "\n", sep = "")
#   output_file <- derived_file(paste0(mn, ".rds"))
#   out <- read_rds(output_file)
#   score <- out$eval_out
#   parameters <- out$parameters
#   extras <- attr(score, "extras")
#   models <- extras$.models
#   summary <- extras$.summary
#   attr(score, "extras") <- NULL
#   write_rds(lst(method_name = mn, parameters, score, models, summary), output_file)
# }

eval_out_real <- lapply(method_names, function(mn) {
  read_rds(derived_file(paste0(mn, ".rds")))
}) %>% list_as_tibble()

combined <- selected_parms %>% full_join(eval_out_real %>% rename(valid_score = score), by = "method_name")

ggplot(combined %>% filter(!method_name %in% c("shuffle", "identity", "random")), aes(test_score, valid_score)) +
  geom_point(aes(colour = train_score)) +
  geom_text(aes(label = method_name), nudge_y = .005) +
  cowplot::theme_cowplot() +
  scale_colour_distiller(palette = "RdBu")

ggplot(combined, aes(train_score, valid_score)) + geom_point() + geom_text(aes(label = method_name), nudge_y = .02) + cowplot::theme_cowplot()

meth_ord <- combined %>% arrange(desc(valid_score)) %>% .$method_name
group_spread <- combined %>%
  select(method_name, train_score, test_score, valid_score) %>%
  gather(group, score, -method_name) %>%
  mutate(group = gsub("_score", "", group), method_f = factor(method_name, levels = rev(meth_ord)))

pdf(figure_file("real_data.pdf"), 6, 6)
ggplot(group_spread) +
  geom_bar(aes(method_f, score), fill = "lightgray", group_spread %>% filter(group == "valid"), stat = "identity") +
  geom_point(aes(method_f, score, colour = group), group_spread %>% filter(group != "valid")) +
  coord_flip() +
  cowplot::theme_cowplot() +
  scale_colour_brewer(palette = "Dark2")+
  scale_fill_brewer(palette = "Dark2")
dev.off()
