source(scripts_file("2a_columns_all.R", experiment = "08-summary"), local = TRUE)

selected_columns <- c("method_name")
selected_groups <- c("benchmark_metric", "benchmark_source", "benchmark_trajtype", "scaling_predtime", "stability", "qc_category")
removed_columns <- c()

column_info <-
  column_info %>%
  filter(id %in% selected_columns | group %in% selected_groups, !id %in% removed_columns)

column_groups <-
  column_groups %>%
  filter(group %in% column_info$group)

