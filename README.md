
<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Build Status](https://api.travis-ci.org/dynverse/dynbenchmark.svg)](https://travis-ci.org/dynverse/dynbenchmark) ![Lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg) <img src="package/man/figures/logo.png" align="right" />

# Benchmarking trajectory inference methods

This repo contains the scripts to reproduce the manuscript

A comparison of single-cell trajectory inference methods: towards more accurate and robust tools
Wouter Saelens\*, Robrecht Cannoodt\*, Helena Todorov, Yvan Saeys
bioRxiv 276907; doi: <https://doi.org/10.1101/276907>

## Structure

This repository is structured as follows:

-   data: raw data files required by the scripts
-   derived: data files produced by the scripts
-   figures: figures created by the scrips
-   manuscript: source files for producing the manuscript
-   package: an R package with helper functions
-   results: data objects outputted by the scripts
-   scripts: code required to reproduce the analyses of the benchmark

## Datasets

The benchmarking pipeline generates (and uses) the following datasets:

-   **Gold standard single-cell datasets**, both real and synthetic, used to evaluated the trajectory inference methods [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.1211533.svg)](https://doi.org/10.5281/zenodo.1211533)

![datasets](package/man/figures/datasets.png)

-   **The performance of methods** used in [dynguidelines](https://www.github.com/dynverse/dynguidelines). *Not yet available*

-   **General information about trajectory inference methods**, available as a data frame in `dynmethods::methods`

## [Scripts](scripts)

The scripts folder contains all the scripts necessary to fully reproduce the benchmarking manuscript in chronologically ordered subfolders. Each subfolder contains a readme file with further explanations of the different scripts and what they do:

| \#  | script/folder                                            | description                                           |
|:----|:---------------------------------------------------------|:------------------------------------------------------|
| 1   | [📁`datasets`](scripts/01-datasets)                       | Dataset processing and characterisation               |
| 2   | [📁`metrics`](scripts/02-metrics)                         | Metrics for comparing two trajectories                |
| 3   | [📁`methods`](scripts/03-methods)                         | Trajectory inference methods                          |
| 4   | [📁`method_testing`](scripts/04-method_testing)           | Method testing                                        |
| 5   | [📁`scaling`](scripts/05-scaling)                         | Scalability of methods                                |
| 6   | [📁`optimise_parameters`](scripts/06-optimise_parameters) | Optimisation of method parameters on synthetic data   |
| 7   | [📁`benchmark`](scripts/07-benchmark)                     | Benchmarking of TI methods on real and synthetic data |
|     | [📁`varia`](scripts/varia)                                |                                                       |

## Benchmarking your own method

Explanation coming soon. Feel free to make an issue or send us an e-mail if you want your method to be included.
