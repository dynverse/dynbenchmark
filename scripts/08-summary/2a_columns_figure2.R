# please don't use the character " within the tribble below
# or other parts of the code will flip out.
# use the character ' instead

column_info <- tribble( # tribble_start
  ~group,                   ~id,                                    ~name,                      ~geom,                         ~palette,
  "method_characteristic",  "method_name",                          "",                         "text(hjust = 0, width = 6)",  NA,
  "method_characteristic",  "method_priors_required_str",           "Priors required",          "text",                        NA,
  "method_characteristic",  "method_wrapper_type",                  "Wrapper type",             "text",                        NA,
  "method_characteristic",  "method_platform",                      "Platform",                 "text",                        NA,
  "method_characteristic",  "method_topology_inference",            "Topology inference",       "text",                        NA,
  "method_characteristic",  "method_most_complex_trajectory_type",  "Most complex traj. type",  "traj",                        NA,
  "inferrable_trajtype",    "itt_cycle",                            "Cycle",                    "traj",                        NA,
  "inferrable_trajtype",    "itt_linear",                           "Linear",                   "traj",                        NA,
  "inferrable_trajtype",    "itt_bifurcation",                      "Bifurcation",              "traj",                        NA,
  "inferrable_trajtype",    "itt_convergence",                      "Convergence",              "traj",                        NA,
  "inferrable_trajtype",    "itt_multifurcation",                   "Multifurcation",           "traj",                        NA,
  "inferrable_trajtype",    "itt_tree",                             "Tree",                     "traj",                        NA,
  "inferrable_trajtype",    "itt_acyclic_graph",                    "Acyclic",                  "traj",                        NA,
  "inferrable_trajtype",    "itt_graph",                            "Connected",                "traj",                        NA,
  "inferrable_trajtype",    "itt_disconnected_graph",               "Disconnected",             "traj",                        NA,
  "score_overall",          "summary_overall_overall",              "Overall",                  "bar(legend = FALSE)",         "overall",
  "score_overall",          "benchmark_overall_overall",            "Benchmark",                "bar(legend = FALSE)",         "benchmark",
  "score_overall",          "scaling_pred_timescore_overall",       "Scalability",              "bar(legend = FALSE)",         "scaling",
  "score_overall",          "qc_overall_overall",                   "QC",                       "bar(legend = FALSE)",         "qc",
  "score_overall",          "control_label",                        "",                         "text(overlay = TRUE)",        NA
) # tribble_end

column_groups <- tribble( # tribble_start
  ~Experiment,  ~Category,                      ~group,
  "Method",     "",                             "method_characteristic",
  "Method",     "Inferrable trajectory types",  "inferrable_trajtype",
  "Summary",    "",                             "score_overall"
) # tribble_end

