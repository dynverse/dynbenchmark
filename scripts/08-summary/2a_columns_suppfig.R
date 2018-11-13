source(scripts_file("2a_columns_all.R", experiment = "08-summary"), local = TRUE)

column_info <-
  column_info %>% filter(!id %in% c("method_most_complex_trajectory_type"))

column_groups <-
  column_groups %>%
  filter(group %in% column_info$group)

