# please don't use the character " within the tribble below
# or other parts of the code will flip out.
# use the character ' instead

column_info <- tribble( # tribble_start
  ~group,                          ~id,                                            ~name,             ~geom,                         ~palette,
  "method_characteristic",         "method_name",                                  "",                "text(hjust = 0, width = 6)",  NA,
  "benchmark_metric",              "benchmark_overall_norm_correlation",           "Ordering",        "circle",                      "benchmark",
  "benchmark_metric",              "benchmark_overall_norm_him",                   "Topology",        "circle",                      "benchmark",
  "benchmark_metric",              "benchmark_overall_norm_featureimp_wcor",       "Features",        "circle",                      "benchmark",
  "benchmark_metric",              "benchmark_overall_norm_F1_branches",           "Clustering",      "circle",                      "benchmark",
  "benchmark_source",              "benchmark_source_real_gold",                   "Gold",            "circle",                      "benchmark",
  "benchmark_source",              "benchmark_source_real_silver",                 "Silver",          "circle",                      "benchmark",
  "benchmark_source",              "benchmark_source_synthetic_dyngen",            "dyngen",          "circle",                      "benchmark",
  "benchmark_source",              "benchmark_source_synthetic_dyntoy",            "dyntoy",          "circle",                      "benchmark",
  "benchmark_source",              "benchmark_source_synthetic_prosstt",           "PROSSTT",         "circle",                      "benchmark",
  "benchmark_source",              "benchmark_source_synthetic_splatter",          "Splatter",        "circle",                      "benchmark",
  "benchmark_trajtype",            "benchmark_tt_cycle",                           "Cycle",           "circle",                      "benchmark",
  "benchmark_trajtype",            "benchmark_tt_linear",                          "Linear",          "circle",                      "benchmark",
  "benchmark_trajtype",            "benchmark_tt_bifurcation",                     "Bifurcation",     "circle",                      "benchmark",
  "benchmark_trajtype",            "benchmark_tt_convergence",                     "Convergence",     "circle",                      "benchmark",
  "benchmark_trajtype",            "benchmark_tt_multifurcation",                  "Multifurcation",  "circle",                      "benchmark",
  "benchmark_trajtype",            "benchmark_tt_tree",                            "Tree",            "circle",                      "benchmark",
  "benchmark_trajtype",            "benchmark_tt_acyclic_graph",                   "Acyclic",         "circle",                      "benchmark",
  "benchmark_trajtype",            "benchmark_tt_graph",                           "Connected",       "circle",                      "benchmark",
  "benchmark_trajtype",            "benchmark_tt_disconnected_graph",              "Disconnected",    "circle",                      "benchmark",
  "benchmark_execution",           "benchmark_overall_pct_errored_str",            "% Errored",       "text(hjust = 1)",             NA,
  "benchmark_execution",           "benchmark_overall_error_reasons",              "Reason",          "pie",                         "error_reasons",
  "scaling_predtime_cells10k",     "scaling_pred_timecomb_cells10k_features1k",    "1k features",     "textbox",                     "scaling",
  "scaling_predtime_cells10k",     "scaling_pred_timecomb_cells10k_features10k",   "10k features",    "textbox",                     "scaling",
  "scaling_predtime_cells10k",     "scaling_pred_timecomb_cells10k_features100k",  "100k features",   "textbox",                     "scaling",
  "scaling_predtime_features10k",  "scaling_pred_timecomb_cells1k_features10k",    "1k cells",        "textbox",                     "scaling",
  "scaling_predtime_features10k",  "scaling_pred_timecomb_cells10k_features10k",   "10k cells",       "textbox",                     "scaling",
  "scaling_predtime_features10k",  "scaling_pred_timecomb_cells100k_features10k",  "100k cells",      "textbox",                     "scaling",
  "qc_category",                   "qc_cat_availability",                          "Availability",    "circle",                      "qc",
  "qc_category",                   "qc_cat_behaviour",                             "Behaviour",       "circle",                      "qc",
  "qc_category",                   "qc_cat_code_assurance",                        "Code assurance",  "circle",                      "qc",
  "qc_category",                   "qc_cat_code_quality",                          "Code quality",    "circle",                      "qc",
  "qc_category",                   "qc_cat_documentation",                         "Documentation",   "circle",                      "qc",
  "qc_category",                   "qc_cat_paper",                                 "Paper",           "circle",                      "qc"
) # tribble_end

column_groups <- tribble( # tribble_start
  ~Experiment,        ~Category,           ~group,
  "Method",           "",                  "method_characteristic",
  "Benchmark",        "Metrics",           "benchmark_metric",
  "Benchmark",        "Sources",           "benchmark_source",
  "Benchmark",        "Trajectory types",  "benchmark_trajtype",
  "Benchmark",        "Execution",         "benchmark_execution",
  "Scalability",      "10k cells",         "scaling_predtime_cells10k",
  "Scalability",      "10k features",      "scaling_predtime_features10k",
  "Quality control",  "Category",          "qc_category"
) # tribble_end

