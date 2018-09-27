source(scripts_file("2a_columns_all.R", experiment = "08-summary"), local = TRUE)

column_info <-
  column_info %>% filter(id == "method_name" | group %in% c("benchmark_metric", "benchmark_source", "benchmark_trajtype", "benchmark_execution", "scaling_predtime_cells10k", "scaling_predtime_features10k", "qc_category"))

column_groups <-
  column_groups %>% filter(group %in% column_info$group)

