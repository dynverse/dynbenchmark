
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![Build
Status](https://api.travis-ci.org/dynverse/dynbenchmark.svg)](https://travis-ci.org/dynverse/dynbenchmark)
![Lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)
<a href = "package/man/figures/logo.svg"><img src="package/man/figures/logo.png" align="right" /></a>
[![doi](https://zenodo.org/badge/doi/10.1101/276907.svg)](https://doi.org/10.1101/276907)

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
> [![altmetric](https://badges.altmetric.com/?size=100&score=101&types=btttttwg&style=bar)](https://altmetric.com/details/33972849)

(Note that this preprint is currently very outdated, see below for more
recent results)

## Experiments

From start to finish, the repository is divided into several
experiments, each with their own scripts and results. These are
accompanied by documentation using github readmes and can thus be easily
explored by going to the appropriate
folders:

| \# | id                       | scripts                                   | results                                                                                        |
| :- | :----------------------- | :---------------------------------------- | :--------------------------------------------------------------------------------------------- |
| 1  | Datasets                 | [📄➡](scripts/01-datasets)                 | [📊➡](https://github.com/dynverse/dynbenchmark_results/tree/master/01-datasets)                 |
| 2  | Metrics                  | [📄➡](scripts/02-metrics)                  | [📊➡](https://github.com/dynverse/dynbenchmark_results/tree/master/02-metrics)                  |
| 3  | Methods                  | [📄➡](scripts/03-methods)                  | [📊➡](https://github.com/dynverse/dynbenchmark_results/tree/master/03-methods)                  |
| 4  | Method testing           | [📄➡](scripts/04-method_testing)           | [📊➡](https://github.com/dynverse/dynbenchmark_results/tree/master/04-method_testing)           |
| 5  | Scaling                  | [📄➡](scripts/05-scaling)                  | [📊➡](https://github.com/dynverse/dynbenchmark_results/tree/master/05-scaling)                  |
| 6  | Benchmark                | [📄➡](scripts/06-benchmark)                | [📊➡](https://github.com/dynverse/dynbenchmark_results/tree/master/06-benchmark)                |
| 7  | Stability                | [📄➡](scripts/07-stability)                | [📊➡](https://github.com/dynverse/dynbenchmark_results/tree/master/07-stability)                |
| 8  | Summary                  | [📄➡](scripts/08-summary)                  | [📊➡](https://github.com/dynverse/dynbenchmark_results/tree/master/08-summary)                  |
| 9  | Guidelines               | [📄➡](scripts/09-guidelines)               | [📊➡](https://github.com/dynverse/dynbenchmark_results/tree/master/09-guidelines)               |
| 10 | Benchmark interpretation | [📄➡](scripts/10-benchmark_interpretation) | [📊➡](https://github.com/dynverse/dynbenchmark_results/tree/master/10-benchmark_interpretation) |
| 11 | Example predictions      | [📄➡](scripts/11-example_predictions)      | [📊➡](https://github.com/dynverse/dynbenchmark_results/tree/master/11-example_predictions)      |
|    | Varia                    | [📄➡](scripts/varia)                       |                                                                                                |

We also have several additional subfolders:

  - [Manuscript](manuscript): Source files for producing the manuscript.
  - [Package](package): An R package with several helper functions.
  - [Raw](raw): Files generated by hand, such as figures and
    spreadsheets.
  - [Derived](derived): Intermediate data files produced by the scripts.
    These files or not git committed.

## Guidelines

Based on the results of the benchmark, we provide context-dependent user
guidelines, [available as a shiny
app](https://github.com/dynverse/dynguidelines). This app is integrated
within the [dyno pipeline](https://github.com/dynverse/dyno), which also
includes the wrappers used in the benchmarking and other packages for
visualising and interpreting the
results.

[![dynguidelines](https://github.com/dynverse/dynguidelines/raw/master/man/figures/demo.gif)](https://github.com/dynverse/dynguidelines)

## Datasets

The benchmarking pipeline generates (and uses) the following datasets:

  - **Gold standard single-cell datasets**, both real and synthetic,
    used to evaluated the trajectory inference methods
    [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.1211533.svg)](https://doi.org/10.5281/zenodo.1211533)

![datasets](package/man/figures/datasets.png)

  - **The performance of methods** used for the [results overview
    figure](NA) and the
    [dynguidelines](https://www.github.com/dynverse/dynguidelines) app.
    *Not yet available*, but can be accessed in the results folder.

  - **General information about trajectory inference methods**,
    available as a data frame in `dynmethods::methods`

## Methods

All wrapped methods are wrapped as both docker and singularity
containers. These can be easily run using
[*dyn*methods](https://github.com/dynverse/dynmethods).

## Benchmarking your own method

Explanation coming soon. Feel free to make an issue or send us an e-mail
if you want your method to be included.
