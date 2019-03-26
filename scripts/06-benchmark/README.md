
# Benchmarking of TI methods on real and synthetic data

| \# | script/folder                                               | description                                                              |
| :- | :---------------------------------------------------------- | :----------------------------------------------------------------------- |
| 1  | [📄`submit_jobs.R`](1-submit_jobs.R)                         | Submit the jobs to the gridengine cluster                                |
| 2  | [📄`retrieve_results.R`](2-retrieve_results.R)               | Retrieve the results from the gridengine cluster and aggregate the data  |
| 3  | [📄`diagnostic_figures.R`](3-diagnostic_figures.R)           | Create some diagnostic figures to get an overview of the results         |
| 3a | [📄`overall_comparison.R`](3a-overall_comparison.R)          | Overall comparison                                                       |
| 3b | [📄`time_mem_predictions.R`](3b-time_mem_predictions.R)      | Comparison between predicted versus actual running time and memory usage |
| 3c | [📄`normalisation.R`](3c-normalisation.R)                    | Compare the effect of normalisation on the results                       |
| 3d | [📄`compare_sources.R`](3d-compare_sources.R)                | Compare the different dataset sources                                    |
|    | [📄`inspect_identity_timings.R`](inspect_identity_timings.R) | Check different benchmark component timings                              |

The results of this experiment are available
[here](https://github.com/dynverse/dynbenchmark_results/tree/master/06-benchmark).
