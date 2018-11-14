
# Scalability of methods

Here we test how well a method scales with increasing number of features
(genes) and/or cells.

Each method is run on down- and upscaled datasets with increasing gene
set and cell set sizes, and the execution times and memory usages are
modelled using thin plate regressions splines within a generalised
additivate
model.

| \# | script/folder                                                 | description                                                                                   |
| :- | :------------------------------------------------------------ | :-------------------------------------------------------------------------------------------- |
| 0  | [📄`generate_data.R`](0-generate_data.R)                       | Generate up- and downscaled datasets                                                          |
| 1  | [📄`submit_jobs.R`](1-submit_jobs.R)                           | Run the methods on the cluster                                                                |
| 2  | [📄`retrieve_results.R`](2-retrieve_results.R)                 | Retrieve the results and generate the scalability models                                      |
| 3  | [📄`generate_figures.R`](3-generate_figures.R)                 | Classify the models and generate scalability figures                                          |
| 3a | [📄`summary_figure.R`](3a-summary_figure.R)                    |                                                                                               |
| 3b | [📄`individual_example.R`](3b-individual_example.R)            |                                                                                               |
| 3c | [📄`individual_overview.R`](3c-individual_overview.R)          |                                                                                               |
| 3d | [📄`error_logs.R`](3d-error_logs.R)                            |                                                                                               |
|    | [📄`compare_model_object_size.R`](compare_model_object_size.R) |                                                                                               |
|    | [📄`generate_dataset.R`](generate_dataset.R)                   | Helper to generate an up- and downscaled dataset which looks similar to the original datasets |

The results of this experiment are available
[here](https://github.com/dynverse/dynbenchmark_results/tree/master/05-scaling).
