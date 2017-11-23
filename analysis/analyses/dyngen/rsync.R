library(tidyverse)
library(dynalysis)

experiment("dyngen")

# update the local dyngen datasets from what is stored on the prism
PRISM:::rsync_remote(
  remote_src = "prism",
  path_src = "/group/irc/shared/dynalysis/analysis/data/",
  remote_dest = "",
  path_dest = paste0(get_dynalysis_folder(), "/analysis/data/")
)
