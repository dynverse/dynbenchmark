
# Method testing

Each method is tested on a few small real and toy datasets. Ideally,
each method should be able to execute successfully on each of these
datasets. If a method is not able to infer a trajectory on any of the
datasets, it will not be included in any of the next experiments.

Overview
experiment:

| \# | script/folder                                               | description                              |
| :- | :---------------------------------------------------------- | :--------------------------------------- |
| 0  | [📄`pull_singularity_images.R`](0-pull_singularity_images.R) | Update singularity images on the cluster |
| 1  | [📄`submit_jobs.R`](1-submit_jobs.R)                         | Submit jobs for method testing           |
| 2  | [📄`retrieve_results.R`](2-retrieve_results.R)               | Retrieve the results                     |
