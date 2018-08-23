# Initialise

## Completely new repo

```
# init the repo
git init
git annex init _name_ --version=6
```

## Common options
```
# Don't copy unlocked files
git config annex.thin true

# Always unlock added files
git config annex.addunlocked true
```

## Setup remotes
```
# cp0480
git annex initremote cp0480 type=rsync rsyncurl=cp0480.irc.ugent.be:/srv/bioit1/data/dyn/benchmark encryption=none exporttree=yes
git annex export master --to cp0480 --tracking

# prism
git remote add prism ssh://prism/group/irc/shared/dynverse/dynbenchmark

git annex list
```

## Cloned repo
```
git clone git@github.com:dynverse/dynbenchmark_results.git results
git annex enableremote cp0480
```

Don't forget to run the common options commands.

```
git annex sync
git annex get
```

# New file

At source:

```
git add
git annex sync --content
```

At destination:
```
git annex sync
git annex get
```

