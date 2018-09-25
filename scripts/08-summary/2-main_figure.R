library(dynbenchmark)
library(tidyverse)
library(dynplot)

experiment("08-summary")

list2env(read_rds(result_file("results.rds", experiment_id = "08-summary")), environment())

# define method groups
method_info <- method_info %>%
  mutate(method_grouping = ifelse(method_source %in% c("tool", "adaptation"), method_most_complex_trajectory_type, method_source))

method_groups <- c(rev(dynwrap::trajectory_types$id), c("adaptation", "offtheshelf", "control"))

# determine palettes
sc_col_fun <- function(palette) {
  function(x) {
    ifelse(is.na(x), "#444444", palette[cut(x, length(palette))])
  }
}
viridis_names <- c("viridis", "magma", "plasma", "inferno", "cividis")
scale_viridis_funs <- map(setNames(viridis_names, viridis_names), ~ sc_col_fun(viridisLite::viridis(101, option = .)))

topinf_colours <- topinf_types %>% select(name, colour) %>% deframe()
error_colours <- error_reasons %>% select(name, colour) %>% deframe()
maxtraj_colours <- trajectory_types %>% select(id, colour) %>% deframe()

exp_palettes <- c(summary = "inferno", qc = "viridis", benchmark = "magma", scalability = "cividis")

# COMBINE PLOT DATA INTO ONE TABLE

#wrapper_type_map <- c(branch_trajectory = "traj", linear_trajectory = "linear", cyclic_trajectory = "cyclic", trajectory = "traj", cell_graph = "cell gr", cluster_graph = "clus gr", control = "control", dimred_projection = "dimred", end_state_probabilities = "prob")
# wrapper_type_map <- c(branch_trajectory = "Traj", linear_trajectory = "Line", cyclic_trajectory = "Cycl", trajectory = "Traj", cell_graph = "Cell", cluster_graph = "Clus", control = "", dimred_projection = "Proj", end_state_probabilities = "Prob")
wrapper_type_map <- c(branch_trajectory = "Traj", linear_trajectory = "Linear", cyclic_trajectory = "Cycle", trajectory = "Traj", cell_graph = "Cell", cluster_graph = "Cluster", control = "", dimred_projection = "Proj", end_state_probabilities = "Prob")
data <-
  bind_rows(
    results %>%
      filter(experiment != "scalability" | category == "overall") %>%
      group_by(experiment, metric, category) %>%
      mutate(value = dynutils::scale_minmax(value)) %>%
      ungroup() %>%
      group_by(experiment) %>%
      mutate(colour = scale_viridis_funs[[exp_palettes[[experiment[[1]]]]]](value)) %>%
      ungroup() %>%
      mutate(
        metric = ifelse(category == "overall" & experiment != "summary", experiment, metric),
        experiment = ifelse(category == "overall" & experiment != "summary", "summary", experiment)
      ),
    results %>% # process scaling results separately
      filter(experiment == "scalability" & category != "overall") %>%
      mutate(colour = scale_viridis_funs[[exp_palettes[["scalability"]]]](value)),
    method_info %>%
      transmute(
        method_id,
        name = method_name,
        control_label = ifelse(method_source == "tool", "", method_source),
        priors = case_when(
          grepl("dataset", method_priors_required) ~ "All",
          grepl("(groups_id|features_id|timecourse_continuous|timecourse_discrete|groups_network)", method_priors_required) ~ "\u2716",
          grepl("(start_id|end_id|end_n|start_n|groups_n)", method_priors_required) ~ "\u2715",
          TRUE ~ ""
        ),
        topology_inference = label_short(ifelse(method_topology_inference == "parameter", "param", method_topology_inference)),
        wrapper_type = wrapper_type_map[method_wrapper_type],
        most_complex = method_most_complex_trajectory_type,
        platform = method_platform,
        tt_cycle = ifelse(method_trajtyp_cycle, "cycle", "gray_cycle"),
        tt_linear = ifelse(method_trajtyp_linear, "linear", "gray_linear"),
        tt_convergence = ifelse(method_trajtyp_convergence, "convergence", "gray_convergence"),
        tt_bifurcation = ifelse(method_trajtyp_bifurcation, "bifurcation", "gray_bifurcation"),
        tt_multifurcation = ifelse(method_trajtyp_multifurcation, "multifurcation", "gray_multifurcation"),
        tt_tree = ifelse(method_trajtyp_tree, "tree", "gray_tree"),
        tt_acyclic_graph = ifelse(method_trajtyp_acyclic_graph, "acyclic_graph", "gray_acyclic_graph"),
        tt_graph = ifelse(method_trajtyp_graph, "graph", "gray_graph"),
        tt_disconnected_graph = ifelse(method_trajtyp_disconnected_graph, "disconnected_graph", "gray_disconnected_graph")
      ) %>%
      gather(metric, label, -method_id) %>%
      mutate(
        experiment = case_when(
          metric == "control_label" ~ "summary",
          TRUE ~ "method"
        ),
        category = case_when(
          grepl("^tt_", metric) ~ "ttinfer",
          TRUE ~ "overall"
        ),
        colour = "black",
        placeholder = FALSE
      )
  )

pie_colours <-
  error_reasons %>% select(metric = name, colour)

# GENERATE METHOD ORDERING
method_ord <-
  data %>%
  filter(experiment == "summary", metric == "overall") %>%
  left_join(method_info, by = "method_id")  %>%
  transmute(method_id, group = factor(method_grouping, levels = method_groups), ranking_score = value) %>%
  arrange(group, desc(ranking_score))

# GENERATE METRIC ORDERING
source(scripts_file("2a-metric_tbl.R"))

metric_annot_names <- tribble(
  ~column, ~value, ~name,
  "experiment", "method", "Method",
  "experiment", "summary", "Summary",
  "experiment", "benchmark", "Benchmark",
  "experiment", "scalability", "Scalability",
  "experiment", "qc", "Quality control",
  "category", "overall", "",
  "category", "ttinfer", "Inferrable trajectory types",
  "category", "metric", "Metrics",
  "category", "sources", "Sources",
  "category", "trajtypes", "Trajectory types",
  "category", "execution", "Execution",
  "category", "stability", "Stability",
  "category", "10k features", "10k features",
  "category", "10k cells", "10k cells",
  "category", "application", "Application",
  "category", "category", "Category"
)

metric_ord <- metric_ord %>%
  left_join(metric_annot_names %>% filter(column == "experiment") %>% select(experiment = value, experiment_name = name), by = "experiment") %>%
  left_join(metric_annot_names %>% filter(column == "category") %>% select(category = value, category_name = name), by = "category")


# GENERATE FIGURE
g <- funky_heatmap(
  data = data,
  metric_ord = metric_ord,
  method_ord = method_ord,
  add_timestamp = TRUE
)

# WRITE FILES
ggsave(result_file("overview.pdf"), g, device = cairo_pdf, width = 26, height = 18)
# ggsave(result_file("overview.svg"), g, width = 20, height = 16)
# ggsave(result_file("overview.png"), g, width = 20, height = 16)


