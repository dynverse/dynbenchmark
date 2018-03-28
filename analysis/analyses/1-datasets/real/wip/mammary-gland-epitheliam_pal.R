rm(list=ls())
library(dynalysis)
library(tidyverse)
library(GEOquery)
library(Biobase)
options('download.file.method.GEOquery'='curl')

id <- "real/mammary-gland-epitheliam_pal"
dataset_preprocessing(id)

geo <- GEOquery::getGEO(GEO = "GSE103272", destdir = dataset_preproc_file(""))


geo
