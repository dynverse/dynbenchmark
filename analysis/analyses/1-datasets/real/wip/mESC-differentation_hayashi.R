rm(list=ls())
library(dynalysis)
library(tidyverse)
library(GEOquery)
library(Biobase)
options('download.file.method.GEOquery'='curl')

dataset_preprocessing("real/mESC-differentiation_hayashi")


# get cell info
geo <- GEOquery::getGEO("GSE98664", destdir = dataset_preproc_file(""))[[1]]
