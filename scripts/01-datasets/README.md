Datasets
================

The datasets are split in real datasets and synthetic datasets. The real datasets are downloaded and preprocessed first, and characteristics from these datasets (such as the number of cells and genes, library sizes, dropout probabilities, ...) are used to generate synthetic datasets. Finally, these dataset are uploaded to zenodo.

All datasets can be downloaded from zenodo using the [00-download\_from\_zenodo.R](00-download_from_zenodo.R) script.

Real datasets
-------------

The generation of the real datasets is divided in two parts:

1.  The datasets are downloaded from their respective sources (such as GEO) using sets of scripts specific for each dataset in [01-download\_from\_sources.R](01-download_from_sources.R) (these scripts are located in [helpers-download\_from\_sources](helpers-download_from_sources))
2.  These datasets are filtered and normalised in [02-filter\_and\_normalise.R](02-filter_and_normalise.R)

Synthetic datasets
------------------
