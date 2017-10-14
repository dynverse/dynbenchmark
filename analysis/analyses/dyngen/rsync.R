
#' update the local dyngen datasets from what is stored on the prism
PRISM:::rsync_remote(
  remote_src = "prism",
  path_src = "/group/irc/shared/dyngen_results/4",
  remote_dest = "",
  path_dest = "/home/rcannood/Workspace/dynverse/dynalysis/analysis/data/derived_data/dyngen/"
)
