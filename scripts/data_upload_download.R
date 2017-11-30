## Sync to prism
PRISM:::rsync_remote("", "analysis/data/", "prism", "/group/irc/shared/dynalysis/analysis/data")
PRISM:::rsync_remote("", "analysis/data/derived_data/datasets/", "prism", "/group/irc/shared/dynalysis/analysis/data/derived_data/datasets")

## Download from prism
PRISM:::rsync_remote("prism", "/group/irc/shared/dynalysis/analysis/data/", "", "analysis/data")
PRISM:::rsync_remote("prism", "/group/irc/shared/dynalysis/analysis/data/datasets/real", "", "analysis/data/datasets/")
