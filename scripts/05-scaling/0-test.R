library(dynbenchmark)
library(tidyverse)

experiment("05-scaling")

##########################################################
###############      DEFINE DATASETS       ###############
##########################################################

source(scripts_file("generate_dataset.R"))

dataset_ids <- c(
  "real/embronic-mesenchyme-neuron-differentiation_mca",
  "real/mouse-cell-atlas-combination-4",
  "real/kidney-collecting-duct-subclusters_park",
  "synthetic/dyngen/25"
)
orig_dataset_id <- dataset_ids[[1]]
lnrow <- log10(1000)
lncol <- log10(1500)
seed <- 1

data <- generate_dataset(orig_dataset_id, lnrow, lncol, seed)

orig_data <- load_dataset(orig_dataset_id)

patchwork::wrap_plots(
  dynplot::plot_graph(orig_data),
  dynplot::plot_dimred(orig_data),
  dynplot::plot_graph(data),
  dynplot::plot_dimred(data),
  ncol = 2
)



out <- evaluate_ti_method(data, ti_scorpius(), metrics = "correlation", parameters = list())

out$summary
model <- out$models[[1]]
