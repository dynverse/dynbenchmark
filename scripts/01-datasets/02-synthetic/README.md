
# Synthetic datasets

Each synthetic dataset is based on some characteristics of some real
datasets. These characteristics include:

  - The number of cells and features
  - The number of features which are differentially expressed in the
    trajectory
  - Estimates of the distribution of the library sizes, average
    expression, dropout probabilities, … estimated by
    [Splatter](https://github.com/Oshlack/splatter).

Here we estimate the parameters of these “platforms” and use them to
simulate datasets using different simulators. Each simulation script
first creates a design dataframe, which links particular platforms,
different topologies, seeds and other parameters specific for a
simulator.

The data is then simulated using wrappers around the simulators (see
[/package/R/simulators.R](/package/R/simulators.R)), so that they all
return datasets in a format consistent with
dynwrap.

| \# | script/folder                                                       | description                                                                                                                 |
| :- | :------------------------------------------------------------------ | :-------------------------------------------------------------------------------------------------------------------------- |
| 1  | [📄`estimate_platform.R`](01-estimate_platform.R)                    | Estimation of the platforms from real data done by `dynbenchmark::estimate_platform`                                        |
| 2a | [📄`simulate_dyngen_datasets.R`](02a-simulate_dyngen_datasets.R)     | [dyngen](https://github.com/dynverse/dyngen), simulations of regulatory networks which will produce a particular trajectory |
| 2b | [📄`simulate_prosstt_datasets.R`](02b-simulate_prosstt_datasets.R)   | [PROSSTT](https://github.com/soedinglab/prosstt), expression is sampled from a linear model which depends on pseudotime     |
| 2c | [📄`simulate_splatter_datasets.R`](02c-simulate_splatter_datasets.R) | [Splatter](https://github.com/Oshlack/splatter), simulations of non-linear paths between different states                   |
| 2d | [📄`simulate_dyntoy_datasets.R`](02d-simulate_dyntoy_datasets.R)     | [dyntoy](https://github.com/dynverse/dyntoy), simulations of toy data using random expression gradients in a reduced space  |
| 3  | [📄`gather_metadata.R`](03-gather_metadata.R)                        | Gathers some metadata about all the synthetic datasets                                                                      |
| 4  | [📄`dyngen_samplers_table.R`](04-dyngen_samplers_table.R)            |                                                                                                                             |
