library(dynbenchmark)
library(tidyverse)

dataset_preprocessing("real/silver/kidney-collecting-duct_park")

txt_location <- download_dataset_source_file(
  "GSE107585_Mouse_kidney_single_cell_datamatrix.txt.gz",
  "https://www.ncbi.nlm.nih.gov/geo/download/?acc=GSE107585&format=file&file=GSE107585%5FMouse%5Fkidney%5Fsingle%5Fcell%5Fdatamatrix%2Etxt%2Egz"
)

df <- read_tsv(txt_location)
df$`AAACCTGAGATATGCA-1`
df[1, ]

overall_cell_info <- tibble(
  cell_id = colnames(df)[2:ncol(df)],
  cluster_number = df[1, 2:ncol(df)] %>% unlist() %>% as.integer()
)

clusters <- tribble(
  ~cluster_number, ~cluster_id,
  6, "CD-PC",
  7, "CD-IC",
  8, "CD-Trans"
)

all_cell_info <- overall_cell_info %>%
  right_join(clusters, "cluster_number")

all_cell_info$subcluster_id <- all_cell_info$cluster_id

all_counts <- df[2:nrow(df), all_cell_info$cell_id] %>% as.matrix() %>% t
colnames(all_counts) <- df[2:nrow(df), 1] %>% unlist()

# extract subclusters
markers <- list(
  "CD-IC-A"=c("Slc4a1", "Aqp6","Adgrf5","Defb1","Mme","Kit","Cbr2","2610016A17Rik","Dmrt2","Slc35g1","Rhbg","Trak1", "Cryab","Scnn1b","Cd24a","Ckb","Clu","Acss1",
"Sh3bgrl3","Epb41l2","Pdlim3","Enah","Pah", "Prss23","Oxgr1","Slc16a11","Nupr1","Rapgef4","Cox7a1","Slc43a2","Klf9","Smco4","Ivns1abp"),
  "CD-IC-B"=c("Slc26a4", "Hmx2", "Nudt4","Spink8","Igfbp5","Insrr","S100a1","Cst3","Emb","Nbea","Plet1"
))
sub_cell_info <- all_cell_info %>%
  filter(cluster_id == "CD-IC") %>%
  mutate(
    A = log2(all_counts[cell_id, markers$`CD-IC-A`]+1) %>% {rowMeans(.>0)},
    B = log2(all_counts[cell_id, markers$`CD-IC-B`]+1) %>% {rowMeans(.>0)},
    subcluster_id = ifelse(A > B*1.5, "CD-IC-A", ifelse(B > A * 1.5, "CD-IC-B", "CD-IC"))
  )

sub_cell_info %>%
  ggplot(aes(A, B, color=factor(subcluster_id))) +
    geom_point()

all_cell_info$subcluster_id[match(sub_cell_info$cell_id, all_cell_info$cell_id)] <- sub_cell_info$subcluster_id

# extract subclusters
markers <- list(
  "CD-PC-PC"=c("Wnk1", "Calb1", "Atp1a1", "Tmem52b", "S100g", "Kl"),
  "CD-PC-CDC"=c("Gstm2", "Aqp2", "Tmsb4x", "S100a11", "Aqp3", "Fxyd4"))
sub_cell_info <- all_cell_info %>%
  filter(cluster_id == "CD-PC") %>%
  mutate(
    A = all_counts[cell_id, markers$`CD-PC-PC`, drop=F] %>% {rowMeans(.>0)},
    B = all_counts[cell_id, markers$`CD-PC-CDC`, drop=F] %>% {rowMeans(.>0)},
    subcluster_id = ifelse(A > B*1.5, "CD-PC-PC", ifelse(B > A * 1.5, "CD-IC-CDC", "CD-IC"))
  )

sub_cell_info %>%
  ggplot(aes(A, B, color=factor(subcluster_id))) +
  geom_point()

all_cell_info$subcluster_id[match(sub_cell_info$cell_id, all_cell_info$cell_id)] <- sub_cell_info$subcluster_id


## Extract datasets
settings <- list(
  list(
    id = "real/silver/kidney-collecting-duct-clusters_park",
    group_id = "cluster_id",
    milestone_network = tribble(
      ~from, ~to,
      "CD-PC", "CD-Trans",
      "CD-Trans", "CD-IC"
    ) %>% mutate(length = 1, directed = FALSE)
  ),
  list(
    id = "real/silver/kidney-collecting-duct-subclusters_park",
    group_id = "subcluster_id",
    milestone_network = tribble(
      ~from, ~to,
      "CD-PC", "CD-PC-PC",
      "CD-PC", "CD-PC-CDC",
      "CD-PC", "CD-Trans",
      "CD-Trans", "CD-IC",
      "CD-IC", "CD-IC-A"
    ) %>% mutate(length = 1, directed = FALSE)
  )
)


for (setting in settings) {
  milestone_network <- setting$milestone_network
  milestone_ids <- unique(c(milestone_network$from, milestone_network$to))

  group_id <- setting$group_id

  cell_info <- all_cell_info %>%
    rename(group_id = !!group_id) %>%
    filter(group_id %in% milestone_ids)

  counts <- all_counts[cell_info$cell_id, ]

  grouping <- cell_info %>% select(cell_id, group_id) %>% deframe()

  save_raw_dataset(lst(milestone_network, cell_info, grouping, counts), setting$id)
}
