library(googledrive)
library(tidyverse)
library(rmarkdown)
library(dynutils)

drive <- drive_download(as_id("1BCCaP21N2PXfzhj9H09yEpZz9lLY2HXd_LxTSsJ_wro"), type="text/plain", overwrite=TRUE, path = tempfile())
system(pritt("sed -i '1s/^.//' {drive$local_path}")) # remove first character, because this is some strang eunicode character
system(pritt("cat analysis/paper/paper_base.Rmd {drive$local_path} > analysis/paper/paper.Rmd"))

render("analysis/paper/paper.Rmd", "html_document")
