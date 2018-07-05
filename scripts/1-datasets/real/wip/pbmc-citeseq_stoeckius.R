# extract the NK cells and ordering according to CD56??
# cannot find the NK cells...


rm(list=ls())
library(dynbenchmark)
library(tidyverse)
library(GEOquery)
library(Biobase)
options('download.file.method.GEOquery'='curl')

id <- "real/pbmc-citeseq_stoeckius"
dataset_preprocessing(id)

geo <- GEOquery::getGEO(GEO = "GSM2695382", destdir = dataset_source_file(""))

rna_location <- download_dataset_source_file(
  "GSE100866_PBMC_vs_flow_10X-RNA_umi.csv.gz",
  "https://www.ncbi.nlm.nih.gov/geo/download/?acc=GSE100866&format=file&file=GSE100866%5FPBMC%5Fvs%5Fflow%5F10X%2DRNA%5Fumi%2Ecsv%2Egz"
)

protein_location <- download_dataset_source_file(
  "GSE100866_PBMC_vs_flow_10X-ADT_umi.csv.gz",
  "https://www.ncbi.nlm.nih.gov/geo/download/?acc=GSE100866&format=file&file=GSE100866%5FPBMC%5Fvs%5Fflow%5F10X%2DADT%5Fumi%2Ecsv%2Egz"
)

counts_rna <- read_csv(rna_location) %>% as.data.frame %>% column_to_rownames("X1") %>% as.matrix() %>% t
colnames(counts_rna) <- gsub("HUMAN_(.*)", "\\1", colnames(counts_rna))
counts_protein <- read_csv(protein_location) %>% as.data.frame %>% column_to_rownames("X1") %>% as.matrix() %>% t



https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSM2695382
