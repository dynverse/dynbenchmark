cat("\n\n")
cat("## Supplementary Figures \n")
cat("\n\n")

plot_figs <- function(figs) {
  if("ggplot" %in% class(figs)) {
    print(figs)
  } else {
    walk(figs, print)
  }
}


subchunkify <- function(ref_id, caption, width=5, height=7) {
  subchunk <- glue::glue("",
                         "",
                         "`r anchor('sfig', '{ref_id}')`",
                         "```{{r {ref_id}, fig.height={height}, fig.width={width}, echo=FALSE, dev='svg', results='asis'}}",
                         "fig <- sfigs %>% filter(ref_id == !!ref_id) %>% pull(fig) %>% .[[1]] %>% plot_figs()",
                         "```",
                         "`r ref('sfig', '{ref_id}')` {caption}",
                         "---",
                         "", .sep = "\n\n")

  cat(knitr::knit(text=subchunk, quiet=TRUE))
}

pwalk(sfigs %>% arrange(match(ref_id, refs$ref_id)), function(ref_id, fig, caption, width, height) {
  subchunkify(ref_id, caption, width, height)
})

cat("\n\n")
cat("## Supplementary Tables \n")
cat("\n\n")

subchunkify <- function(ref_id, caption, width=5, height=7) {
  subchunk <- glue::glue("",
                         "",
                         "`r ref('stable', '{ref_id}', anchor=TRUE)` {caption}",
                         "```{{r {ref_id}, echo=FALSE}}",
                         "stables %>% filter(ref_id == !!ref_id) %>% pull(table) %>% .[[1]]",
                         "```",
                         "---",
                         "",
                         "", .sep = "\n")

  cat(knitr::knit(text=subchunk, quiet=TRUE))
}

pwalk(stables %>% arrange(match(ref_id, refs$ref_id)), function(ref_id, table, caption) {
  subchunkify(ref_id, caption)
})
