library(dynalysis)
library(tidyverse)
library(dynplot)

experiment("5-optimise_parameters/5-plot_all_trajectories")

#### FIRST RUN THE RSYNC FROM SCRIPT 3C ####

list2env(read_rds(derived_file("outputs_postprocessed.rds", "5-optimise_parameters/3-evaluate_parameters")), environment())
list2env(read_rds(derived_file("config.rds", "5-optimise_parameters/3-evaluate_parameters")), environment())

tasks <- map_df(paste0(local_tasks_folder, "/", task_ids, ".rds"), read_rds) %>% filter(task_source != "toy")
methods <- dynmethods::get_ti_methods()
method_names <- methods$short_name %>% discard(~ . %in% c("identity", "shuffle", "random", "manual"))

walk(tasks$id, function(tid) {
  dir.create(derived_file(tid), recursive = T, showWarnings = F)

  task <- tasks %>% filter(id == tid) %>% extract_row_to_list(1)

  cat("Processing ", tid, "\n", sep = "")
  walk(method_names, function(mid) {
    method <- methods %>% filter(short_name == mid)

    debug_out <- outputs_ind %>% filter(method_short_name == mid, task_id == tid) %>% arrange(desc(rank_correlation))

    cat("  ", tid, "; ", mid, "\n", sep = "")
    pdf(derived_file(paste0(tid, "/", mid, ".pdf")), 30, 12)
    walk(seq_len(nrow(debug_out)), function(i) {
      tryCatch({
        model_id <- debug_out$model_id[[i]]
        model <- load_dyneval_model(method_short_name = mid, model_id = model_id, experiment_id = "5-optimise_parameters/3-evaluate_parameters")[[1]]

        wp_cells <- unique(c(model$waypoint_cells, task$waypoint_cells))
        task_geo <- dynwrap::compute_tented_geodesic_distances(task, wp_cells)
        model_geo <- dynwrap::compute_tented_geodesic_distances(model, wp_cells)

        # g_geo <-
        #   if (task$task_source != "real") {
        #     ggplot() + geom_jitter(aes(as.vector(task_geo), as.vector(model_geo)), width = .05, height = 0.05, alpha = .5)
        #   } else {
        #     ggplot() + geom_violin(aes(factor(as.vector(task_geo)), as.vector(model_geo)))
        #   }
        #
        # g_geo <- g_geo + cowplot::theme_cowplot() + labs(x = "Task geodesic", y = "Prediction geodesic", title = "Comparison of geodesic distances")

        g_own <-
          tryCatch({
            method$plot_fun[[1]](model)
          }, error = function(e) {
            g <- ggplot() + geom_point(aes(1, 1), alpha = 0)
            process_dynplot(g, "")
          })

        g_traj <- cowplot::plot_grid(
          plot_default(task) + labs(title = "Task"),
          plot_default(model) + labs(title = "Prediction"),
          g_own + labs(title = "Method's own plot"),
          plot_combined(model, task) + labs(title = "Task topology\nwith prediction colouring"),
          plot_combined(task, model) + labs(title = "Prediction topology\nwith task colouring"),
          nrow = 2
        )

        g_strip <- plot_strip_connections(task, model)

        g_comb <- cowplot::plot_grid(
          # g_geo,
          g_traj,
          g_strip,
          nrow = 1,
          rel_widths = c(3, 2)
        )

        title <- cowplot::ggdraw() + cowplot::draw_label(glue::glue("{mid}; {tid}; replicate {i}"), fontface='bold')
        cowplot::plot_grid(title, g_comb, ncol=1, rel_heights=c(0.1, 1)) %>% print()
      }, error = function(e) {
        cat("Error at ", tid, ", ", mid, ", ", i, ": ", as.character(e$message), "\n", sep = "")
      })
    })
    dev.off()
  })
})


