source(scripts_file("2a_columns_all.R", experiment = "08-summary"), local = TRUE)

removed_groups <- c("qc_application", "scaling_predmem")

column_info <-
  column_info %>% filter(!id %in% c("method_most_complex_trajectory_type"), !group %in% removed_groups)

column_groups <-
  column_groups %>%
  filter(group %in% column_info$group)

