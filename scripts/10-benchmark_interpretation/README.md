
# Benchmark interpretation

Various attempts at interpreting the results of the
benchmark.

| \# | script/folder                                                                | description                                                                                                                 |
| :- | :--------------------------------------------------------------------------- | :-------------------------------------------------------------------------------------------------------------------------- |
| 1  | [📄`variability.R`](01-variability.R)                                         | The variability of the benchmarking results across dataset sources and trajectory types                                     |
| 2  | [📄`dataset_sources.R`](02-dataset_sources.R)                                 | Similarity of the results across dataset sources                                                                            |
| 3a | [📄`topology_statistics.R`](03a-topology_statistics.R)                        | Calculate some statistics about the topologies predicted by the methods                                                     |
| 3b | [📄`topology_complexity.R`](03b-topology_complexity.R)                        | Compare the complexity of the topologies predicted by the methods                                                           |
| 4  | [📄`complementarity.R`](04-complementarity.R)                                 | Try to find good combinations of methods given a set of datasets, which could indicate some complementarity between methods |
| 5  | [📄`benchmark_interpretation_figure.R`](05-benchmark_interpretation_figure.R) | Combine the different figures of this experiment into one                                                                   |
| 6  | [📄`compare_timings.R`](06-compare_timings.R)                                 | Compare the predicted execution times and memory usages with the actual ones                                                |
|    | [📄`helper-complementarity.R`](helper-04-complementarity.R)                   | Helper plotting functions the complementarity experiment                                                                    |
|    | [📄`scaling_fit_cv.R`](scaling_fit_cv.R)                                      | Test different models for scalability, using the running times from the benchmark as validation set                         |
