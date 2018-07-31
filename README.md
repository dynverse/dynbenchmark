
<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Build Status](https://api.travis-ci.org/dynverse/dynbenchmark.svg)](https://travis-ci.org/dynverse/dynbenchmark) ![Lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg) <img src="package/man/figures/logo.png" align="right" />

Benchmarking single-cell trajectory inference methods
=====================================================

Scripts to reproduce the manuscript

A comparison of single-cell trajectory inference methods: towards more accurate and robust tools
Wouter Saelens\*, Robrecht Cannoodt\*, Helena Todorov, Yvan Saeys
bioRxiv 276907; doi: <https://doi.org/10.1101/276907>

Structure
---------

This repository is structured as follows:

-   data: raw data files required by the scripts
-   derived: data files produced by the scripts
-   figures: figures created by the scrips
-   manuscript: source files for producing the manuscript
-   package: an R package with helper functions
-   results: data objects outputted by the scripts
-   scripts: code required to reproduce the analyses of the benchmark

Datasets
--------

The benchmarking pipeline generates (and uses) the following datasets:

-   **Gold standard single-cell datasets**, both real and synthetic, used to evaluated the trajectory inference methods [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.1211533.svg)](https://doi.org/10.5281/zenodo.1211533)

![datasets](package/man/figures/datasets.png)

-   **The performance of methods** used in [dynguidelines](https://www.github.com/dynverse/dynguidelines). *Not yet available*

-   **General information about trajectory inference methods**, available as a data frame in `dynmethods::methods`

Scripts
-------

The scripts folder contains all the scripts necessary to fully reproduce the benchmarking manuscript in chronologically ordered subfolders. Each subfolder contains a readme file with further explanations of the different scripts and what they do:

| order | subfolder                                                                                                             | description                                                |
|:------|:----------------------------------------------------------------------------------------------------------------------|:-----------------------------------------------------------|
| 1     | [datasets](https://github.com/dynverse/dynbenchmark/tree/master/scripts/01-datasets)                                  | Generation of datasets                                     |
| 2     | [dataset\_characterisation](https://github.com/dynverse/dynbenchmark/tree/master/scripts/02-dataset_characterisation) | Characterisation of datasets                               |
| 3     | [metric\_characterisation](https://github.com/dynverse/dynbenchmark/tree/master/scripts/03-metric_characterisation)   | Metrics characterisation and testing                       |
| 4     | [method\_characterisation](https://github.com/dynverse/dynbenchmark/tree/master/scripts/04-method_characterisation)   | Methods characterisation and testing                       |
| 5     | [scaling](https://github.com/dynverse/dynbenchmark/tree/master/scripts/05-scaling)                                    | Scalability assessments of the methods on toy data         |
| 6     | [optimise\_parameters](https://github.com/dynverse/dynbenchmark/tree/master/scripts/06-optimise_parameters)           | Optimisation of parametersof the methods on synthetic data |
| 7     | [benchmark](https://github.com/dynverse/dynbenchmark/tree/master/scripts/07-benchmark)                                | Benchmarking of methods on real and synthetic data         |
| 8     | [aggregate](https://github.com/dynverse/dynbenchmark/tree/master/scripts/08-aggregate)                                |                                                            |
| 9     | [user\_guidelines](https://github.com/dynverse/dynbenchmark/tree/master/scripts/09-user_guidelines)                   | User guidelines generation based on the benchmark          |
| 12    | [varia](https://github.com/dynverse/dynbenchmark/tree/master/scripts/12-varia)                                        |                                                            |
|       | [old](https://github.com/dynverse/dynbenchmark/tree/master/scripts/old)                                               |                                                            |

Benchmarking your own method
----------------------------

Explanation coming soon. Feel free to make an issue or send us an e-mail if you want your method to be included.
