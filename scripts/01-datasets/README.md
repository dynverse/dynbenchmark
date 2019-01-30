
# Dataset processing and characterisation

The datasets are split in real datasets and synthetic datasets. The real
datasets are downloaded and preprocessed first, and characteristics from
these datasets (such as the number of cells and genes, library sizes,
dropout probabilities, …) are used to generate synthetic datasets. The
datasets are then characterised, after which they are uploaded to
Zenodo.

| \# | script/folder                                              | description                                                                                                       |
| :- | :--------------------------------------------------------- | :---------------------------------------------------------------------------------------------------------------- |
| 0  | [📄`download_from_zenodo.R`](00-download_from_zenodo.R)     | Downloading the processed datasets from Zenodo ([10.5281/zenodo.1443566](https://doi.org/10.5281/zenodo.1443566)) |
| 1  | [📁`real`](01-real)                                         | Real datasets                                                                                                     |
| 2  | [📁`synthetic`](02-synthetic)                               | Synthetic datasets                                                                                                |
| 3  | [📄`download_from_prism.R`](03-download_from_prism.R)       | Download the datasets from the cluster                                                                            |
| 4  | [📁`dataset_characterisation`](04-dataset_characterisation) | Dataset characterisation                                                                                          |
| 5  | [📄`upload_to_zenodo.R`](05-upload_to_zenodo.R)             | Upload the datasets to Zenodo ([10.5281/zenodo.1211532](https://doi.org/10.5281/zenodo.1211532))                  |
|    | [📄`hotfix_datasets.R`](hotfix_datasets.R)                  |                                                                                                                   |

The results of this experiment are available
[here](https://github.com/dynverse/dynbenchmark_results/tree/master/01-datasets).

## [Real datasets](01-real)

The generation of the real datasets is divided in two parts. We first
download all the (annotated) expression files from sites such as GEO.
Next, we filter and normalise all datasets, and wrap them into the
common trajectory format of
[dynwrap](https://www.github.com/dynverse/dynwrap).

| \# | script/folder                                                             | description                                                                                                                                                                                         |
| :- | :------------------------------------------------------------------------ | :-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| 1  | [📄`download_from_sources.R`](01-real/01-download_from_sources.R)          | Downloading the real datasets from their sources (eg. GEO), and constructing the gold standard model, using the helpers in [helpers-download\_from\_sources](01-real/helpers-download_from_sources) |
| 2  | [📄`filter_and_normalise.R`](01-real/02-filter_and_normalise.R)            | Filtering and normalising the real datasets using `dynbenchmark::process_raw_dataset` All datasets are then saved into the dynwrap format.                                                          |
| 3  | [📄`gather_metadata.R`](01-real/03-gather_metadata.R)                      | Gathers some metadata about all the real datasets                                                                                                                                                   |
| 4  | [📄`datasets_table.R`](01-real/04-datasets_table.R)                        | Creates a table of the datasets in, excuse me, excel (for supplementary material)                                                                                                                   |
|    | [📁`helpers-download_from_sources`](01-real/helpers-download_from_sources) |                                                                                                                                                                                                     |

## [Synthetic datasets](02-synthetic)

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

| \# | script/folder                                                                    | description                                                                                                                 |
| :- | :------------------------------------------------------------------------------- | :-------------------------------------------------------------------------------------------------------------------------- |
| 1  | [📄`estimate_platform.R`](02-synthetic/01-estimate_platform.R)                    | Estimation of the platforms from real data done by `dynbenchmark::estimate_platform`                                        |
| 2a | [📄`simulate_dyngen_datasets.R`](02-synthetic/02a-simulate_dyngen_datasets.R)     | [dyngen](https://github.com/dynverse/dyngen), simulations of regulatory networks which will produce a particular trajectory |
| 2b | [📄`simulate_prosstt_datasets.R`](02-synthetic/02b-simulate_prosstt_datasets.R)   | [PROSSTT](https://github.com/soedinglab/prosstt), expression is sampled from a linear model which depends on pseudotime     |
| 2c | [📄`simulate_splatter_datasets.R`](02-synthetic/02c-simulate_splatter_datasets.R) | [Splatter](https://github.com/Oshlack/splatter), simulations of non-linear paths between different states                   |
| 2d | [📄`simulate_dyntoy_datasets.R`](02-synthetic/02d-simulate_dyntoy_datasets.R)     | [dyntoy](https://github.com/dynverse/dyntoy), simulations of toy data using random expression gradients in a reduced space  |
| 3  | [📄`gather_metadata.R`](02-synthetic/03-gather_metadata.R)                        | Gathers some metadata about all the synthetic datasets                                                                      |
| 4  | [📄`dyngen_samplers_table.R`](02-synthetic/04-dyngen_samplers_table.R)            |                                                                                                                             |

\`

## [Dataset characterisation](04-dataset_characterisation)

Characterisation of the datasets regarding the different topologies
present.

| \# | script/folder                                              | description                                               |
| :- | :--------------------------------------------------------- | :-------------------------------------------------------- |
| 1  | [📄`topology.R`](04-dataset_characterisation/01-topology.R) | An overview of all the topologies present in the datasets |
