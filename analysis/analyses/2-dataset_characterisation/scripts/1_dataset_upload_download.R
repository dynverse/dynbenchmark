## Sync to prism
PRISM:::rsync_remote("", paste0("data/", .version, "/"), "prism", paste0("/group/irc/shared/dyngen_results/", .version, "/"))

## Download from prism
PRISM:::rsync_remote("prism", "/group/irc/shared/dyngen_results/results", "", "~/Workspace/papers/ti_eval/dyngen")
