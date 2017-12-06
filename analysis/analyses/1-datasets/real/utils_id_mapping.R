library(tidyverse)
experiment("normalisation")

ensembl_human <- org.Hs.eg.db::org.Hs.egENSEMBL2EG %>% as.list() %>% unlist() %>% {tibble(ensembl = names(.), entrez=.)}
symbol_human <- org.Hs.eg.db::org.Hs.egSYMBOL %>% as.list() %>% unlist() %>% {tibble(entrez = names(.), symbol=.)}
human_mapper <- full_join(ensembl_human, symbol_human, by="entrez")

ensembl_mouse <- org.Mm.eg.db::org.Mm.egENSEMBL2EG %>% as.list() %>% unlist() %>% {tibble(ensembl = names(.), entrez=.)}
symbol_mouse <- org.Mm.eg.db::org.Mm.egSYMBOL %>% as.list() %>% unlist() %>% {tibble(entrez = names(.), symbol=.)}
mouse_mapper <- full_join(ensembl_mouse, symbol_mouse, by="entrez")

id_mapper <- bind_rows(human_mapper, mouse_mapper) %>%
  group_by(entrez) %>%
  filter(row_number() == 1)

saveRDS(id_mapper, derived_file("id_mapper.rds"))
