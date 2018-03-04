library(googledrive)
library(tidyverse)
library(rmarkdown)
library(dynutils)

drive <- drive_download(as_id("1BCCaP21N2PXfzhj9H09yEpZz9lLY2HXd_LxTSsJ_wro"), type="text/plain", overwrite=TRUE, path = tempfile())
system(pritt("sed -i '1s/^.//' {drive$local_path}")) # remove first character, because this is some strange unicode character
system(pritt("cat {drive$local_path} > analysis/paper/paper.Rmd"))

# add wip
read_file("analysis/paper/paper.Rmd") %>%
  str_replace_all("§(.*?)\n", "<p class='wip'>\\1</p>") %>%
  str_replace_all("\\[[a-z]\\]", "") %>%
  str_replace_all("→", "-->") %>%
  write_file("analysis/paper/paper.Rmd")

##  ............................................................................
##  HTML                                                                    ####

file.remove("analysis/paper/paper.html")

params <- list(table_format="html")
render(
  "analysis/paper/paper.Rmd",
  output_format = bookdown::gitbook(split_by="none", number_sections=FALSE, config=list(toc=list(scroll_highlight = "yes")), css="style.css", lib_dir = ".scratch/"),
  output_dir = "analysis/paper/",
  output_file = "paper.html",
  params = c(table_format="html")
)
browseURL("analysis/paper/paper.html")

##  ............................................................................
##  PDF                                                                     ####

# create pdfs and pngs for every svg, svg conversion is not really good in pandoc
files <- list.files("analysis/figures", recursive=TRUE, full.names=T) %>% keep(endsWith, ".svg")
# tofiles <- list.files("analysis/figures", recursive=TRUE, full.names=T) %>% keep(endsWith, ".svg") %>% gsub("\\.svg", "\\.tmp\\.png", .)
# parallel::mclapply(purrr::transpose(list(files, tofiles)), function(.) system(glue::glue("inkscape {.[[1]]} --export-png={.[[2]]}")), mc.cores=8)
tofiles <- list.files("analysis/figures", recursive=TRUE, full.names=T) %>% keep(endsWith, ".svg") %>% gsub("\\.svg", "\\.tmp\\.pdf", .)
parallel::mclapply(purrr::transpose(list(files, tofiles)), function(.) system(glue::glue("inkscape {.[[1]]} --export-pdf={.[[2]]}")), mc.cores=8)

# generate pdfs

# create pdf. 1. remove all temp files 2. make sure pdfs are used for all figures 3. Global parameters
file.remove(list.files("analysis/paper/", "paper_latex*", full.names = T))
read_file("analysis/paper/paper.Rmd") %>% gsub("\\.svg", "\\.tmp\\.pdf", .) %>% write_file("analysis/paper/paper_latex.Rmd")
params <- list(table_format="latex")
render(
  "analysis/paper/paper_latex.Rmd",
  output_format = rmarkdown::pdf_document(
    latex_engine = "xelatex",
    dev="png",
    keep_tex=TRUE,
    citation_package="biblatex",
    pandoc_args=c("--csl=nature-biotechnology.csl")
  ),
  output_dir = "analysis/paper/",
  output_file = "paper_latex.pdf",
  clean=F
)
# unlink(tofiles) # remove tmp pdfs

# upload to google drive
drive_update("dynverse/paper.pdf", "analysis/paper/paper_latex.pdf")
