saveRDS(qsub_environment, "env.rds")
PRISM:::rsync_remote("", "env.rds", "prism", "/group/irc/personal/wouters/env.rds")
saveRDS(paramsets, "paramsets.rds")
PRISM:::rsync_remote("", "paramsets.rds", "prism", "/group/irc/personal/wouters/paramsets.rds")


##

list2env(as.list(readRDS("env.rds")), .GlobalEnv)
paramsets <- readRDS("paramsets.rds")
prepare_environment()
params <- paramsets[[1]]