library(dynalysis)
library(tidyverse)
library(dynplot)

list2env(read_rds(derived_file("outputs_postprocessed.rds", "5-optimise_parameters/3-evaluate_parameters")), environment())
list2env(read_rds(derived_file("config.rds", "5-optimise_parameters/3-evaluate_parameters")), environment())

tasks <- read_rds(derived_file("tasks.rds", "2-dataset_characterisation"))
methods <- dynmethods::get_descriptions()



task_ids <- c("synthetic/linear_2", "real/distal-lung-epithelium_treutlein", "synthetic/consecutive_bifurcating_5")
method_ids <- c("identity", "scorpius", "slngsht", "mnclddr", "tscan", "mfa", "mpath", "ctmaptpx", "ctgibbs", "ctvem", "gpfates", "dpt", "wishbone")

outputs_oi <- outputs_ind %>% filter(task_id %in% task_ids, repeat_i == 1, method_short_name %in% method_ids)
outputs_oi$model <- map2(outputs_oi$method_short_name, outputs_oi$model_id, function(method_short_name, model_id) {
  load_dyneval_model(
    method_short_name = method_short_name,
    model_id = model_id,
    experiment_id = "5-optimise_parameters/3-evaluate_parameters"
  )[[1]]
})
outputs_oi <- outputs_oi %>% filter(!map_dbl(model, is.null))

scores <- outputs_oi %>% split(outputs_oi$task_id) %>% map(function(outputs_oi) {
  design <- t(combn(nrow(outputs_oi), 2)) %>%
    as.data.frame()
  design <- bind_cols(
    outputs_oi[design$V1, ] %>% rename_all(~paste0(., "_from")),
    outputs_oi[design$V2, ] %>% rename_all(~paste0(., "_to"))
  )

  scores <- pbapply::pblapply(cl=8, seq_len(nrow(design)), function(row_i) {
    dyneval::calculate_metrics(design$model_from[[row_i]], design$model_to[[row_i]])
  }) %>% bind_rows()

  scores <- bind_cols(design, scores)
  scores <- bind_rows(
    scores,
    scores %>% rename(
      tmp = method_short_name_from,
      method_short_name_from = method_short_name_to,
      method_short_name_to = method_short_name_from
    )
  )
  scores
}) %>% bind_rows()



similarity <- reshape2::acast(
  scores,
  method_short_name_from~method_short_name_to,
  value.var="edge_flip",
  fun.aggregate=mean
)
similarity[is.na(similarity)] <- 1

clust <- hclust(as.dist(1-similarity))
clust %>% plot()

space <- cmdscale(as.dist(1-similarity)) %>% as.data.frame() %>% tibble::rownames_to_column("method_short_name") %>% rename(Comp1=V1, Comp2=V2)
space %>%
  ggplot(aes(Comp1, Comp2)) +
    geom_point() +
    ggrepel::geom_label_repel(aes(label=method_short_name))


similarity %>% pheatmap::pheatmap()
