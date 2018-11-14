
# Stability

Analysis of the stability of each method: how similar are the
trajectories when rerunning a method on slightly perturbed
data?

| \# | script/folder                                 | description                                         |
| :- | :-------------------------------------------- | :-------------------------------------------------- |
| 0  | [📄`generate_data.R`](0-generate_data.R)       | Generate the subsampled datasets                    |
| 1  | [📄`submit_jobs.R`](1-submit_jobs.R)           | Submit the stability jobs                           |
| 2  | [📄`retrieve_results.R`](2-retrieve_results.R) | Retrieve and process the stability results          |
|    | [📄`generate_dataset.R`](generate_dataset.R)   | Helper function for generating a subsampled dataset |

The results of this experiment are available
[here](https://github.com/dynverse/dynbenchmark_results/tree/master/07-stability).
