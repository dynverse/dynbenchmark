library(dynalysis)
library(tidyverse)
library(dynplot)

experiment("5-optimise_parameters/9-plot_dataset-bifurcating_5")

#### FIRST RUN THE RSYNC FROM SCRIPT 3C ####

list2env(read_rds(derived_file("outputs_postprocessed.rds", "5-optimise_parameters/3-evaluate_parameters")), environment())
list2env(read_rds(derived_file("config.rds", "5-optimise_parameters/3-evaluate_parameters")), environment())

tasks <- read_rds(derived_file("tasks.rds", "2-dataset_characterisation"))
methods <- dynmethods::get_descriptions()
#method_names <- methods$short_name %>% discard(~ . %in% c("identity", "shuffle", "random", "manual"))
method_names <- c("wishbone", "dpt", "phenopth", "ctvem", "tscan")

set.seed(1)
tids <- c("synthetic/bifurcating_5", sample(tasks$id, 10))

walk(tids, function(tid) {
  dir.create(derived_file(tid), recursive = T, showWarnings = F)

  task <- tasks %>% filter(id == tid) %>% extract_row_to_list(1)


  cat("Processing ", tid, "\n", sep = "")
  walk(method_names, function(mid) {
    method <- methods %>% filter(short_name == mid)

    debug_out <- outputs_ind %>% filter(method_short_name == mid, task_id == tid) %>% arrange(desc(rank_correlation))

    cat("  ", tid, "; ", mid, "\n", sep = "")
    pdf(derived_file(paste0(tid, "/", mid, ".pdf")), 24, 15)
    walk(seq_len(nrow(debug_out)), function(i) {
      tryCatch({
        model_id <- debug_out$model_id[[i]]
        model <- load_dyneval_model(method_short_name = mid, model_id = model_id, experiment_id = "5-optimise_parameters/3-evaluate_parameters")[[1]]

        wp_cells <- unique(c(model$waypoint_cells, task$waypoint_cells))
        task_geo <- dynwrap::compute_tented_geodesic_distances(task, wp_cells)
        model_geo <- dynwrap::compute_tented_geodesic_distances(model, wp_cells)

        task_colours <- dynplot:::check_or_perform_dimred(task, insert_phantom_edges = TRUE) %>% .$space_samples %>% {setNames(.$colour, .$cell_id)}
        model_colours <- dynplot:::check_or_perform_dimred(model, insert_phantom_edges = TRUE) %>% .$space_samples %>% {setNames(.$colour, .$cell_id)}

        g_own <-
          tryCatch({
            method$plot_fun[[1]](model)
          }, error = function(e) {
            g <- ggplot() + geom_point(aes(1, 1), alpha = 0)
            process_dynplot(g, "")
          })
        g_own1 <-
          tryCatch({
            method$plot_fun[[1]](model, custom_cell_colour = task_colours)
          }, error = function(e) {
            g <- ggplot() + geom_point(aes(1, 1), alpha = 0)
            process_dynplot(g, "")
          })
        g_own2 <-
          tryCatch({
            method$plot_fun[[1]](model, custom_cell_colour = model_colours)
          }, error = function(e) {
            g <- ggplot() + geom_point(aes(1, 1), alpha = 0)
            process_dynplot(g, "")
          })

        g_traj <- cowplot::plot_grid(
          plot_default(task) + labs(title = "Task"),
          plot_default(model) + labs(title = "Prediction"),
          plot_combined(model, task) + labs(title = "Task topology\nwith prediction colouring"),
          plot_combined(task, model) + labs(title = "Prediction topology\nwith task colouring"),
          g_own1 + labs(title = "Method's own plot with prediction colouring"),
          g_own2 + labs(title = "Method's own plot with task colouring"),
          nrow = 3
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
