source(scripts_file("2a_columns_all.R", experiment = "08-summary"), local = TRUE)

selected_groups <- c("method_characteristic", "inferrable_trajtype", "score_overall")
removed_columns <- c("method_most_complex_trajectory_type", "itt_convergence", "itt_acyclic_graph")
column_info <-
  column_info %>%
  filter(group %in% selected_groups, !id %in% removed_columns)

column_groups <-
  column_groups %>% filter(group %in% column_info$group)

