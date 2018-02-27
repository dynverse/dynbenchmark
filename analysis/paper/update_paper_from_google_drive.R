library(googledrive)
library(tidyverse)
library(rmarkdown)
library(dynutils)

drive <- drive_download(as_id("1BCCaP21N2PXfzhj9H09yEpZz9lLY2HXd_LxTSsJ_wro"), type="text/plain", overwrite=TRUE, path = tempfile())
system(pritt("sed -i '1s/^.//' {drive$local_path}")) # remove first character, because this is some strange unicode character
system(pritt("cat {drive$local_path} > analysis/paper/paper.Rmd"))

# add wip
read_file("analysis/paper/paper.Rmd") %>% gsub("§(.*?)\n", "<p class='wip'>\\1</p>", .) %>% write_file("analysis/paper/paper.Rmd")

# remove comments
read_file("analysis/paper/paper.Rmd") %>% gsub("\\[[a-z]\\]", "", .) %>% write_file("analysis/paper/paper.Rmd")


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
read_file("analysis/paper/paper.Rmd") %>% gsub("\\.svg", "\\.pdf", .) %>% write_file("analysis/paper/paper.Rmd")

file.remove("analysis/paper/paper.pdf")

params <- list(table_format="latex")
render(
  "analysis/paper/paper.Rmd",
  output_format = rmarkdown::pdf_document(
    latex_engine = "xelatex",
    dev="png",
    keep_tex=TRUE
  ),
  output_dir = "analysis/paper/",
  output_file = "paper.pdf",
  clean=F
)

# upload to google drive
drive_update("dynverse/paper.pdf", "analysis/paper/paper.pdf")


# for conversion to pdf:
# system(pritt("cd analysis/paper; pandoc paper.html +RTS -K1024M -RTS --pdf-engine=xelatex -o paper.pdf"))



files <- list.files("analysis/figures", recursive=TRUE, full.names=T) %>% keep(endsWith, ".svg")
tofiles <- list.files("analysis/figures", recursive=TRUE, full.names=T) %>% keep(endsWith, ".svg") %>% gsub("\\.svg", "\\.png", .)
walk2(files, tofiles, ~system(glue::glue("inkscape {.} --export-png={.y}")))
tofiles <- list.files("analysis/figures", recursive=TRUE, full.names=T) %>% keep(endsWith, ".svg") %>% gsub("\\.svg", "\\.pdf", .)
walk2(files, tofiles, ~system(glue::glue("inkscape {.} --export-pdf={.y}")))
