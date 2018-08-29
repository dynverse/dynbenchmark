library(googledrive)
library(tidyverse)
library(rmarkdown)
library(dynutils)

drive <- drive_download(as_id("14ZzuesLq5u5l-Gp_r5tSkvwOpxxz9LuXS_HG6GAYfxw"), type="text/plain", overwrite=TRUE, path = tempfile())
system(pritt("sed -i '1s/^.//' {drive$local_path}")) # remove first character, because this is some strange unicode character
system(pritt("cat {drive$local_path} > manuscript/paper.Rmd"))

read_file("manuscript/paper.Rmd") %>%
  str_replace_all("\n\\[[a-z]{1,2}\\][^\n]*", "") %>%
  str_replace_all("\\[[a-z]{1,2}\\]", "") %>%
  str_replace_all("→", "-->") %>%
  str_replace_all("\n#", "\n\n#") %>% # add double new line before (sub)titles
  write_file("manuscript/paper.Rmd")

# process all svgs
files <- list.files("figures", recursive=TRUE, full.names=T) %>% keep(endsWith, ".svg")
walk(files, function(file) {
  svg <- xml2::read_xml(file)
  if (is.null(xml2::xml_attr(svg, "width"))) {
    viewBox <- xml2::xml_attr(svg, "viewBox") %>% str_split(" ") %>% first() %>% as.numeric()
    if (length(viewBox) > 1) {
      svg %>% xml2::xml_set_attr("width", viewBox[[3]])
      svg %>% xml2::xml_set_attr("height", viewBox[[4]])
      svg %>% xml2::write_xml(file)
    }
  }
})

##  ............................................................................
##  HTML                                                                    ####
file.remove("analysis/paper/paper.html")

params <- list(table_format="html")
render(
  "analysis/paper/paper.Rmd",
  output_format = bookdown::gitbook(split_by="none", number_sections=FALSE, config=list(toc=list(scroll_highlight = "yes")), css="style.css", lib_dir = ".scratch/"),
  output_dir = "analysis/paper/",
  output_file = "paper.html",
  params = c(table_format = "html")
)
browseURL("analysis/paper/paper.html")

##  ............................................................................
##  PDF                                                                     ####

# create pdfs and pngs for every svg, svg conversion is not really good in pandoc
files <- list.files("figures", recursive=TRUE, full.names=T) %>% keep(endsWith, ".svg")
tofiles <- files %>% gsub("\\.svg", "\\.tmp\\.pdf", .)
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
    dev = "png",
    keep_tex = TRUE,
    citation_package = "biblatex",
    pandoc_args = c("--csl=nature-biotechnology.csl")
  ),
  output_dir = "analysis/paper/",
  output_file = "paper_latex.pdf",
  clean = F
)
render(
  "analysis/paper/supplementary_latex.Rmd",
  output_format = rmarkdown::pdf_document(
    latex_engine = "xelatex",
    dev = "png",
    keep_tex = TRUE,
    citation_package = "biblatex",
    pandoc_args = c("--csl=nature-biotechnology.csl")
  ),
  output_dir = "analysis/paper/",
  output_file = "supplementary_latex.pdf",
  clean = F
)
# unlink(tofiles) # remove tmp pdfs

# upload to google drive
drive_update("dynverse/paper.pdf", "analysis/paper/paper_latex.pdf")
drive_update("dynverse/supplementary.pdf", "analysis/paper/supplementary_latex.pdf")

# prepare for submission
paper_folder <- "../../dyndocs/20180401_submission_nat_biotech/"
file.copy("analysis/paper/paper_latex.pdf", paste0(paper_folder, "paper.pdf"), overwrite=TRUE)
file.copy("analysis/paper/supplementary_latex.pdf", paste0(paper_folder, "supplementary_material.pdf"), overwrite=TRUE)

figs <- read_rds(derived_file("figs.rds", "paper"))
figs$fig_id <- seq_len(nrow(figs))

walk2(figs$fig_id, figs$fig_path, function(fig_id, fig_path) {
  file.copy(fig_path, paste0(paper_folder, glue::glue("fig_{fig_id}.pdf")), overwrite=TRUE)
})
