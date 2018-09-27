library(dynbenchmark)
library(tidyverse)
library(dynplot)

experiment("08-summary")

####################################
###         PROCESS DATA         ###
####################################
method_groups <- c(rev(dynwrap::trajectory_types$id), c("adaptation", "offtheshelf", "control"))

wrapper_type_map <- c(branch_trajectory = "Traj", linear_trajectory = "Linear", cyclic_trajectory = "Cycle", trajectory = "Traj", cell_graph = "Cell", cluster_graph = "Cluster", control = "", dimred_projection = "Proj", end_state_probabilities = "Prob")

data <-
  read_rds(result_file("results.rds", experiment_id = "08-summary")) %>%
  rename(id = method_id) %>%
  mutate(
    group = ifelse(method_source %in% c("tool", "adaptation"), method_most_complex_trajectory_type, method_source),
    group = factor(group, levels = method_groups),
    control_label = ifelse(method_source == "tool", "", method_source),
    method_priors_required_str = case_when(
      grepl("dataset", method_required_priors_str) ~ "All",
      grepl("(groups_id|features_id|timecourse_continuous|timecourse_discrete|groups_network)", method_required_priors_str) ~ "\u2716",
      grepl("(start_id|end_id|end_n|start_n|groups_n)", method_required_priors_str) ~ "\u2715",
      TRUE ~ ""
    ),
    method_topology_inference = label_short(ifelse(method_topology_inference == "parameter", "param", method_topology_inference)),
    method_wrapper_type = wrapper_type_map[method_wrapper_type],
    benchmark_overall_error_reasons = pmap(
      lst(err = benchmark_overall_pct_errored, time = benchmark_overall_pct_time_limit, mem = benchmark_overall_pct_memory_limit),
      function(err, time, mem) {
        c(
          pct_memory_limit = mem,
          pct_time_limit = time,
          pct_method_error = err - mem - time
        )
      }
    ),
    benchmark_overall_pct_errored_str = ifelse(benchmark_overall_pct_errored < .01, "<1%", paste0(round(benchmark_overall_pct_errored * 100), "%"))
  ) %>%
  arrange(group, desc(summary_overall_overall))

for (col in stringr::str_subset(colnames(data), "^scaling_pred_timestr_")) {
  col_score <- gsub("timestr", "timescore", col)
  col_comb <- gsub("timestr", "timecomb", col)
  data[[col_comb]] <- map2(data[[col]], data[[col_score]], function(l, v) list(value = v, label = l))
}

for (tt in dynwrap::trajectory_types$id) {
  data[[paste0("itt_", tt)]] <- ifelse(data[[paste0("method_detects_", tt)]], tt, paste0("gray_", tt))
}

####################################
###  DETERMINE METHOD GROUPING   ###
####################################
row_info <-
  data %>%
  select(group, id)
row_groups <-
  data %>%
  transmute(Group = label_short(group), group) %>%
  unique()

####################################
###   DETERMINE METRIC GROUPING  ###
####################################
script_file <- scripts_file("2a-metric_tbl.R")
reformat_tribbles(script_file, "l")
source(script_file)



####################################
###        CREATE FIGURE         ###
####################################
g <-
  funky_heatmap(
    data,
    column_info,
    column_groups,
    row_info,
    row_groups,
    palettes,
    add_timestamp = TRUE
  )

# WRITE FILES
ggsave(result_file("overview.pdf"), g, device = cairo_pdf, width = 26, height = 18)
# ggsave(result_file("overview.svg"), g, width = 20, height = 16)
# ggsave(result_file("overview.png"), g, width = 20, height = 16)


