#' Helper for creating small images for each rule

library(dynbenchmark)
library(esfiji)

experiment("02-metrics/02-metric_conformity")

svg_location <- raw_file("perturbations.svg")

folder <- result_file("images")
dir.create(folder)
svg_groups_split(svg_location, folder = folder)
