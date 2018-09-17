
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![Build
Status](https://api.travis-ci.org/dynverse/dynbenchmark.svg)](https://travis-ci.org/dynverse/dynbenchmark)
![Lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)
<a href = "package/man/figures/logo.svg"><img src="package/man/figures/logo.png" align="right" /></a>

# Benchmarking trajectory inference methods

This repo contains the scripts to reproduce the manuscript

> A comparison of single-cell trajectory inference methods: towards more
> accurate and robust tools  
> <strong> Wouter Saelens\* </strong>
> <a href='https://orcid.org/0000-0002-7114-6248'><img src='https://github.com/dynverse/dynmethods/raw/master/man/figures/orcid_logo.svg?sanitize=true' height='16'></a>
> <a href='https://github.com/zouter'><img src='https://github.com/dynverse/dynmethods/raw/master/man/figures/github_logo.png' height='16'></a>,
> <strong> Robrecht Cannoodt\* </strong>
> <a href='https://orcid.org/0000-0003-3641-729X'><img src='https://github.com/dynverse/dynmethods/raw/master/man/figures/orcid_logo.svg?sanitize=true' height='16'></a>
> <a href='https://github.com/rcannood'><img src='https://github.com/dynverse/dynmethods/raw/master/man/figures/github_logo.png' height='16'></a>,
> Helena Todorov
> <a href='https://github.com/Helena-todd'><img src='https://github.com/dynverse/dynmethods/raw/master/man/figures/github_logo.png' height='16'></a>,
> <em> Yvan Saeys </em>
> <a href='https://github.com/saeyslab'><img src='https://github.com/dynverse/dynmethods/raw/master/man/figures/github_logo.png' height='16'></a>  
> [bioRxiv:276907](https://www.biorxiv.org/content/early/2018/03/05/276907)
> [doi:10.1101/276907](https://doi.org/10.1101/276907)

## Experiments

From start to finish, the repository is divided into several
experiments, each with their own scripts and results. These are
accompanied by documentation using github READMEs and can be easily
explored by going to the appropriate
folders:

| \# | id                  | scripts                                                       | results                                                                                                  |
| :- | :------------------ | :------------------------------------------------------------ | :------------------------------------------------------------------------------------------------------- |
| 1  | Datasets            | [Datasets scripts](scripts/01-datasets)                       | [Datasets results](https://github.com/dynverse/dynbenchmark_results/tree/master/01-datasets)             |
| 2  | Metrics             | [Metrics scripts](scripts/02-metrics)                         | [Metrics results](https://github.com/dynverse/dynbenchmark_results/tree/master/02-metrics)               |
| 3  | Methods             | [Methods scripts](scripts/03-methods)                         | [Methods results](https://github.com/dynverse/dynbenchmark_results/tree/master/03-methods)               |
| 4  | Method testing      | [Method testing scripts](scripts/04-method_testing)           | [Method testing results](https://github.com/dynverse/dynbenchmark_results/tree/master/04-method_testing) |
| 5  | Scaling             | [Scaling scripts](scripts/05-scaling)                         | [Scaling results](https://github.com/dynverse/dynbenchmark_results/tree/master/05-scaling)               |
| 6  | Optimise parameters | [Optimise parameters scripts](scripts/06-optimise_parameters) |                                                                                                          |
| 7  | Benchmark           | [Benchmark scripts](scripts/07-benchmark)                     | [Benchmark results](https://github.com/dynverse/dynbenchmark_results/tree/master/07-benchmark)           |
| 8  | Summary             | [Summary scripts](scripts/08-summary)                         | [Summary results](https://github.com/dynverse/dynbenchmark_results/tree/master/08-summary)               |
| 9  | Guidelines          | [Guidelines scripts](scripts/09-guidelines)                   |                                                                                                          |
|    | Varia               | [Varia scripts](scripts/varia)                                |                                                                                                          |

We also have several additional subfolders:

  - manuscript: source files for producing the manuscript
  - package: an R package with several helper functions
  - data: raw data files required by the scripts
  - derived: intermediate data files produced by the scripts

## Datasets

The benchmarking pipeline generates (and uses) the following datasets:

  - **Gold standard single-cell datasets**, both real and synthetic,
    used to evaluated the trajectory inference methods
    [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.1211533.svg)](https://doi.org/10.5281/zenodo.1211533)

![datasets](package/man/figures/datasets.png)

  - **The performance of methods** used in
    [dynguidelines](https://www.github.com/dynverse/dynguidelines). *Not
    yet available*

  - **General information about trajectory inference methods**,
    available as a data frame in `dynmethods::methods`

## Benchmarking your own method

Explanation coming soon. Feel free to make an issue or send us an e-mail
if you want your method to be included.

## Guidelines

Based on the results of the benchmark, we provide context-dependent user
guidelines, [available as a shiny
app](https://github.com/dynverse/dynguidelines). This app is integrated
within the [dyno pipeline](https://github.com/dynverse/dyno), which also
includes the wrappers used in the benchmarking and other packages for
visualising and interpreting the
results.

[![dynguidelines](https://github.com/dynverse/dynguidelines/raw/master/man/figures/demo.gif)](https://github.com/dynverse/dynguidelines)
