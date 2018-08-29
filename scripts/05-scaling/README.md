
# Scalability of methods

Here we test how well a method scales with increasing number of features (genes) and/or cells.

Each method is run on randomly generated data with increasing gene set and cell set sizes, and the execution times and memory usages are modelled using linear models.

| \#  | script                                        | description |
|:----|:----------------------------------------------|:------------|
| 0   | [📄`generate_data.R`](0-generate_data.R)       |             |
| 1   | [📄`submit_jobs.R`](1-submit_jobs.R)           |             |
| 2   | [📄`retrieve_results.R`](2-retrieve_results.R) |             |
| 3   | [📄`generate_figures.R`](3-generate_figures.R) |             |
|     | [📄`generate_dataset.R`](generate_dataset.R)   |             |
