Datasets
================

The datasets are split in real datasets and synthetic datasets. The real datasets are downloaded and preprocessed first, and characteristics from these datasets (such as the number of cells and genes, library sizes, dropout probabilities, ...) are used to generate synthetic datasets. Finally, these dataset are uploaded to zenodo.

All datasets can be downloaded from zenodo using the [00-download\_from\_zenodo.R](00-download_from_zenodo.R) script.

Real datasets
-------------

The generation of the real datasets is divided in two parts:

1.  The datasets are downloaded from their respective sources (such as GEO) using sets of scripts specific for each dataset in [01-download\_from\_sources.R](01-download_from_sources.R) (these scripts are located in [helpers-download\_from\_sources](helpers-download_from_sources))
2.  These datasets are filtered and normalised in [02-filter\_and\_normalise.R](02-filter_and_normalise.R). This is done using the dynbenchmark::process\_raw\_dataset function.

Following datasets were ultimately generated:

``` r
# library(tidyverse)
# library(dynbenchmark)
# list_datasets() %>% 
#   filter(source == "real") %>% 
#   select(name)
```

Synthetic datasets
------------------

Each synthetic dataset is based on some characteristics of some real datasets. These characteristics include:

-   The number of cells and features
-   The number of features which are differentially expressed in the trajectory
-   Estimates of the distribution of the library sizes, average expression, dropout probabilities, ... estimated by [Splatter](https://github.com/Oshlack/splatter)

These are estimated in [01-estimate\_platform.R](01-estimate_platform.R) and are called "platforms".

Next, we simulate datasets using different simulators:

-   [Dyngen](https://github.com/dynverse/dyngen), simulations of regulatory networks which will produce a particular trajectory
-   [PROSSTT](https://github.com/soedinglab/prosstt), simulations of tree topologies using random walks
-   [Splatter](https://github.com/Oshlack/splatter), simulations of non-linear paths between different states
-   [Dynplot](https://github.com/dynverse/dynplot), simulations of toy data using random expression gradients in a reduced space

Each simulation script (02a-02d) first creates a design dataframe, which links particular platforms, different topologies, seeds and other parameters specific for a simulator.

The data is then simulated using wrappers around the simulators (see [/package/R/simulators.R](/package/R/simulators.R)), so that they all return datasets in a format consistent with dynwrap.
