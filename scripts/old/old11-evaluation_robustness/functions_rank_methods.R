rank_methods <- function(outputs_ind) {
  ### PART 1
  # aggregate over replicates
  outputs_summrepl <- outputs_ind %>%
    group_by(method_short_name, dataset_id, dataset_source, paramset_id, trajectory_type, dataset_source, prior_str, trajectory_type_f) %>%
    summarise_if(is.numeric, mean) %>%
    ungroup() %>%
    mutate(
      pct_allerrored = (pct_other_error == 1)+0,
      pct_stochastic = pct_other_error - pct_allerrored,
      harm_mean = apply(cbind(norm_correlation, norm_edge_flip, norm_rf_mse), 1, psych::harmonic.mean)
    )

  # process trajtype grouped evaluation
  outputs_summtrajtype <- outputs_summrepl %>%
    group_by(method_short_name, dataset_source, paramset_id, trajectory_type, trajectory_type_f) %>%
    mutate(n = n()) %>%
    summarise_if(is.numeric, mean) %>%
    ungroup() %>%
    mutate(
      harm_mean = apply(cbind(norm_correlation, norm_edge_flip, norm_rf_mse), 1, psych::harmonic.mean)
    )

  # process overall evaluation
  outputs_summmethod <- outputs_summtrajtype %>%
    group_by(method_short_name, dataset_source, paramset_id) %>%
    mutate(n = n()) %>%
    summarise_if(is.numeric, mean) %>%
    ungroup() %>%
    mutate(
      harm_mean = apply(cbind(norm_correlation, norm_edge_flip, norm_rf_mse), 1, psych::harmonic.mean)
    )

  # adding mean per trajtype
  outputs_summtrajtype_totals <- bind_rows(
    outputs_summtrajtype,
    outputs_summtrajtype %>%
      group_by(method_short_name, paramset_id, trajectory_type, trajectory_type_f) %>%
      summarise_if(is.numeric, mean) %>%
      ungroup() %>%
      mutate(dataset_source = "mean")
  ) %>%
    mutate(
      harm_mean = apply(cbind(norm_correlation, norm_edge_flip, norm_rf_mse), 1, psych::harmonic.mean)
    )

  # adding mean per method
  outputs_summmethod_totals <-
    bind_rows(
      outputs_summmethod,
      outputs_summmethod %>%
        group_by(method_short_name, paramset_id) %>%
        summarise_if(is.numeric, mean) %>%
        ungroup() %>%
        mutate(dataset_source = "mean")
    ) %>%
    mutate(
      harm_mean = apply(cbind(norm_correlation, norm_edge_flip, norm_rf_mse), 1, psych::harmonic.mean)
    )

  # combine all aggregated data frames
  outputs_summtrajtype_totalsx2 <- bind_rows(
    outputs_summmethod_totals %>% mutate(trajectory_type = "overall"),
    outputs_summtrajtype_totals
  ) %>%
    mutate(trajectory_type_f = factor(trajectory_type, levels = trajectory_types$id)) %>%
    mutate(
      harm_mean = apply(cbind(norm_correlation, norm_edge_flip, norm_rf_mse), 1, psych::harmonic.mean)
    )


  ##### PART 2

  part_overall_mean <-
    outputs_summtrajtype_totalsx2 %>%
    filter(trajectory_type == "overall", dataset_source == "mean") %>%
    select(
      method_short_name, harm_mean, norm_correlation, norm_edge_flip, norm_rf_mse, time_method, rank_time_method,
      num_files_created, num_setseed_calls, pct_errored, pct_time_exceeded, pct_memory_exceeded, pct_allerrored, pct_stochastic
    ) %>%
    mutate(overall_metric = apply(.[,colnames(.)[grepl("^norm_", colnames(.))]], 1, mean))

  part_sources <-
    outputs_summtrajtype_totalsx2 %>%
    filter(trajectory_type == "overall", dataset_source != "mean") %>%
    select(method_short_name, dataset_source, harm_mean) %>%
    mutate(dataset_source = paste0("source_", dataset_source)) %>%
    spread(dataset_source, harm_mean) %>%
    mutate(overall_source = apply(.[,colnames(.)[grepl("^source_", colnames(.))]], 1, mean))

  part_trajtypes <-
    outputs_summtrajtype_totalsx2 %>%
    filter(trajectory_type != "overall", dataset_source == "mean") %>%
    select(method_short_name, trajectory_type, harm_mean) %>%
    mutate(trajectory_type = paste0("trajtype_", trajectory_type)) %>%
    spread(trajectory_type, harm_mean) %>%
    mutate(overall_trajtype = apply(.[,colnames(.)[grepl("^trajtype_", colnames(.))]], 1, mean))

  # part_complexities <-
  #   complexities$complexity %>%
  #   select(
  #     method_short_name, complexity_inferred = feature, complexity_inferred2 = feature2, complexity_sign = sign, complexity_rsq = rsq
  #   )
  #
  # part_variances <-
  #   variances$vardf %>%
  #   select(
  #     method_short_name, mean_var, var_norm_correlation, var_norm_edge_flip, var_norm_rf_mse, sets_seeds
  #   )

  part_priors <-
    outputs_ind %>%
    select(method_short_name, prior_str) %>%
    distinct()

  ## COMBINE_COLUMNS
  part_list <- lst(
    part_overall_mean,
    part_sources,
    part_trajtypes
  )
  reduce_fun <- function(a, b) inner_join(a, b, by = "method_short_name")
  method_tib <- Reduce("reduce_fun", part_list) %>%
    mutate(
      overall_benchmark = apply(cbind(overall_metric, overall_source, overall_trajtype), 1, psych::harmonic.mean)
    ) %>%
    inner_join(read_rds(derived_file("methods.rds", "04-method_characterisation")) %>% filter(type %in% c("control", "algorithm")), "method_short_name")

  method_tib
}
