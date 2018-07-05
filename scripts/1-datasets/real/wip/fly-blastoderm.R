library(tidyverse)
library(dynbenchmark)

dataset_preprocessing("real/fly-blastoderm-atac_cusanovich")

txt_location <- download_dataset_source_file(
  "2to4_files.tar.gz",
  "http://krishna.gs.washington.edu/content/members/cusanovich/fly_embryogenesis/updated_data/vignette/2to4_files.tar.gz"
)
system(paste0("tar -xzf ", txt_location, " -C ", dataset_source_file(""), " --strip-components=1"))



cds_2to4 = readRDS(dataset_source_file("2to4_files/cds_2to4_aggregated.rds"))
overlapped_sites = read.table(dataset_source_file('./2to4_files/2to4.overlapped_sites.bed'))
cell_classification = read.table(dataset_source_file('./2to4_files/2to4.germlayers.txt'))
table(cell_classification$V2)



DA_monocle_list = lapply(seq(1,7,1), FUN = function(x) {
  DA_file_name = paste0(dataset_source_file('./2to4_files/2to4.sigopen.cluster'), x, '.txt')
  DA_file = read.csv(DA_file_name, sep = '\t', header = F)
  DA_file$GL = x
  return (DA_file)
})

DA_monocle = data.table::rbindlist(DA_monocle_list)
colnames(DA_monocle) = c('chr','start', 'end', 'qvalue', 'GL')
DA_monocle$coord = paste(DA_monocle$chr, DA_monocle$start, DA_monocle$end, sep = '_')

get_top_DA = function(cluster = 1, DA_results=DA_GL, num = 500){
  DA_this_cluster = subset(DA_results, GL == cluster)
  DA_this_cluster = DA_this_cluster[order(DA_this_cluster[,qvalue]),]
  if (num > nrow(DA_this_cluster)){
    return (DA_this_cluster[c(1:nrow(DA_this_cluster)),]$coord)
  }else {return (DA_this_cluster[c(1:num),]$coord)}
}

DA_sites_to_order = unlist(lapply(c(1,3,4,5,6), get_top_DA, DA_results = DA_monocle,num=100))
overlapped_DA_sites_to_order = subset(overlapped_sites, V4 %in% DA_sites_to_order)$V8


rownames(cell_classification) = cell_classification$V1
germ_layer_pallet = c('Unknown'='#68228B', 'Blastoderm'='#808080', 'Neural'='#1F78B4', 'Ectoderm'='#FFD700',
                      'Mesoderm'= '#E31A1C', 'Endoderm'='#60CC52', 'Collisions'='saddlebrown')
cell_classification = cell_classification[rownames(pData(cds_2to4)),]
cds_2to4$germ_layer_name = cell_classification$V2
fData(cds_2to4)$use_for_ordering = FALSE
cds_2to4 = setOrderingFilter(cds_2to4, overlapped_DA_sites_to_order)


