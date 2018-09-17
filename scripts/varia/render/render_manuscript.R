#' Renders the manuscript

rmarkdown::render("manuscript/paper.Rmd", output_format = pdf_manuscript())
system("/usr/bin/xdg-open manuscript/paper.pdf")
