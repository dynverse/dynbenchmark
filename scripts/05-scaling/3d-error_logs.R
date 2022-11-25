##########################################################
###############     GENERATE ERROR LOGS    ###############
##########################################################
method_ids <- unique(data$method_id) %>% setdiff("error")

last_lines <- function(s, num) {
  s %>%
    stringr::str_split(pattern = "\n") %>%
    first() %>%
    keep(~ . != "") %>%
    tail(num) %>%
    paste(collapse = "\n")
}
cluster <- function(s, num_diffs) {
  clusters <- c(1, rep(NA, length(s) - 1))
  for (i in seq_len(length(s) - 1)) {
    cli <- clusters[seq_len(i)]
    ixs <- map_int(unique(cli), ~ first(which(cli == .)))
    dists <- utils::adist(s[ixs], s[i+1])[,1]
    ix <- ixs[dists <= num_diffs]
    clusters[[i+1]] <-
      if (length(ix) > 0) {
        cli[[ix[[1]]]]
      } else {
        max(clusters, na.rm = TRUE) + 1
      }
  }
  clusters
}

error_statuses <- unique(data$error_status)

datasets_info <- data %>% select(dataset_id, orig_dataset_id, lnrow, lncol)

pbapply::pblapply(method_ids, cl = 8, function(mid) {
  selection <-
    data %>%
    filter(dataset_id %in% scaling_avail, method_id == mid, error_status != "no_error") %>%
    mutate(
      error_text = paste0(stdout, "\n", stderr, "\n", error_message)
    )

  sta_lines <- map(error_statuses, function(sta) {
    sel_sta <- selection %>% filter(error_status == sta)

    if (nrow(sel_sta) == 0) {
      return(NULL)
    }

    if (nrow(sel_sta) > 2) {
      clusts <-
        sel_sta$error_text %>%
        map_chr(last_lines, num = 5) %>%
        str_replace_all("Rtmp[^\\n ]*", "") %>%
        str_replace_all("[^a-zA-Z]", "") %>%
        cluster(num_diffs = 10)
    } else if (nrow(sel_sta) == 1) {
      clusts <- 1
    } else {
      clusts <- c()
    }

    cluster_lines <- unlist(lapply(unique(clusts), function(cl) {
      sel_cl <- sel_sta %>% slice(which(clusts == cl))

      g <-
        ggplot(sel_cl) +
        geom_rect(aes(xmin = lnrow - .1, xmax = lnrow + .1, ymin = lncol - .1, ymax = lncol + .1), datasets_info %>% anti_join(sel_cl, by = "dataset_id"), fill = "lightgray") +
        geom_rect(aes(xmin = lnrow - .1, xmax = lnrow + .1, ymin = lncol - .1, ymax = lncol + .1, fill = error_status)) +
        scale_fill_manual(values = dynbenchmark::method_status_colours) +
        scale_x_nrow + scale_y_ncol +
        theme_classic() +
        theme(legend.position = "bottom") +
        labs(x = "# cells", y = "# features", fill = "Status") +
        facet_wrap(~ orig_dataset_id, nrow = 1) +
        coord_equal()

      ggsave(result_file(c("results/error_class_plots/", mid, "_", sta, "_", cl, ".png")), g, width = 10, height = 2.8)

      vals <- sel_cl %>% dynutils::extract_row_to_list(1)
      c(
        "### ERROR CLUSTER ", sta %>% toupper(), " -- ", cl, "\n",
        "![Cluster plot](error_class_plots/", mid, "_", sta, "_", cl, ".png)\n",
        "\n",
        " * Number of instances: ", nrow(sel_cl), "\n",
        " * Dataset ids: ", paste(sel_cl$dataset_id, collapse = ", "), "\n",
        "\n",
        "Last 10 lines of ", sel_cl$dataset_id[[1]], ":\n",
        "```\n",
        last_lines(vals$error_text, num = 10), "\n",
        "```\n",
        "\n"
      )
    }))

    paste0(
      c(
        "## ERROR STATUS ", sta %>% toupper(), "\n",
        "\n",
        cluster_lines
      ),
      collapse = ""
    )
  })


  lines <- paste0(c(
    "# ", mid, "\n",
    "![Overview](", mid, ".png)\n",
    "\n",
    unlist(sta_lines)
  ), collapse = "")

  readr::write_lines(lines, result_file(c("results/", mid, "_overview.md")))

  invisible()
})


