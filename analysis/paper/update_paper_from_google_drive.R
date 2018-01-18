library(googledrive)
library(tidyverse)
library(rmarkdown)
library(dynutils)

drive <- drive_download(as_id("1BCCaP21N2PXfzhj9H09yEpZz9lLY2HXd_LxTSsJ_wro"), type="text/plain", overwrite=TRUE, path = tempfile())
system(pritt("sed -i '1s/^.//' {drive$local_path}")) # remove first character, because this is some strange unicode character
system(pritt("cat {drive$local_path} > analysis/paper/paper.Rmd"))

file.remove("analysis/paper/paper.html")
render("analysis/paper/paper.Rmd", "html_document", output_dir = "analysis/paper/", output_file = "paper.html")
browseURL("analysis/paper/paper.html")
