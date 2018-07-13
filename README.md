
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

We include the following datasets. When using any of these datasets, please also cite our preprint: [![DOI](https://zenodo.org/badge/DOI/10.1101/276907.svg)](https://doi.org/10.1101/276907)

-   [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.1211533.svg)](https://doi.org/10.5281/zenodo.1211533) Single-cell -omics datasets, both real and synthetic, used to evaluated the trajectory inference methods
-   **Coming soon** Main results of the evaluation, used to rank the methods and construct practical guidelines
