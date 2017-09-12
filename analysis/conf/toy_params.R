linear:



# choose certain parameters for each method, at which we know this method will perform well for the toy dataset
method_params_linear <- list(
  waterfall=list(),
  scorpius=list(),
  slingshot=list(),
  gpfates=list(nfates=1, log_expression_cutoff=),
  stemid=list(clustnr=10, bootnr=10, pdishuf=10),
  tscan=list(),
  embeddr=list(nn_pct = 2),
  celltree_gibbs=list(sd_filter = 0),
  celltree_maptpx=list(sd_filter = 0),
  celltree_vem=list(sd_filter = 0),
  scuba=list(),
  slicer = list(min_branch_len=50),
  monocle_ddrtree=list(),
  wishbone = list(branch=F),
  pseudogp = list(iter=100, initialise_from="principal_curve"),
  mpath = list(numcluster=6),
  scoup = list(nbranch=1),
  slice = list(),
  ouija = list(),
  random_linear=list()
)

method_params_bifurcating <- list(
  waterfall=list(),
  scorpius=list(),
  slingshot=list(),
  gpfates=list(nfates=2),
  stemid=list(clustnr=10, bootnr=10, pdishuf=10),
  tscan=list(),
  embeddr=list(nn_pct = 2),
  celltree_gibbs=list(sd_filter = 0),
  celltree_maptpx=list(sd_filter = 0),
  celltree_vem=list(sd_filter = 0),
  scuba=list(),
  slicer = list(min_branch_len=50),
  monocle_ddrtree=list(),
  wishbone = list(branch=F),
  pseudogp = list(iter=100, initialise_from="principal_curve"),
  mpath = list(numcluster=6),
  scoup = list(nbranch=2),
  random_linear=list()
)
