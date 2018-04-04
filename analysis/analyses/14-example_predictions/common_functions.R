plot_task_cells <- function(task) {
  source("../dynmodular/dimred_wrappers.R")
  space_cells <- dimred_mds(task$counts(), ndim=2) %>%
    as_tibble() %>%
    mutate(cell_id = rownames(task$expression())) %>%
    left_join(task$prior_information$grouping_assignment, by="cell_id") %>%
    left_join(groups)

  # define space groups
  space_groups <- space_cells %>%
    group_by(group_id) %>%
    summarise_if(is.numeric, mean)

  # add missing groups (if no cells were added)
  space_groups <- bind_rows(
    map_df(
      setdiff(groups$group_id, space_groups$group_id),
      function(group_id) {
        close_group_ids <-
          c(
            milestone_network %>%
              filter(from == group_id) %>%
              pull(to),
            milestone_network %>%
              filter(to == group_id) %>%
              pull(from) %>%
              rep(3)
          )


        space_groups %>%
          slice(match(close_group_ids, group_id)) %>%
          summarise_if(is.numeric, mean) %>%
          mutate(group_id = !!group_id)
      }),
    space_groups
  )
  space_groups <- space_groups %>%
    left_join(groups)

  # group network
  group_network <- milestone_network %>%
    left_join(
      space_groups %>% rename_all(~paste0(., "_from")),
      by=c("from" = "group_id_from")
    ) %>%
    left_join(
      space_groups %>% rename_all(~paste0(., "_to")),
      by=c("to" = "group_id_to")
    ) %>%
    mutate(
      Comp1_mid = Comp1_from + (Comp1_to - Comp1_from) /2,
      Comp2_mid = Comp2_from + (Comp2_to - Comp2_from) /2
    )

  library(tidygraph)
  library(ggraph)

  task_method_plot <- space_cells %>%
    ggplot(aes(Comp1, Comp2)) +
    geom_point(aes(fill=color), color="black", shape=21) +
    geom_edge_link(aes(x=Comp1_from, y=Comp2_from, xend=Comp1_to, yend=Comp2_to), data=group_network) +
    geom_edge_link(aes(x=Comp1_from, y=Comp2_from, xend=Comp1_mid, yend=Comp2_mid), data=group_network, arrow=arrow(type="closed", length = unit(0.2, "cm"))) +
    geom_point(color="black", data=space_groups, size=6) +
    geom_point(aes(color=color), data=space_groups, size=4) +
    scale_color_identity() +
    scale_fill_identity() +
    theme_minimal()
  task_method_plot <- process_dynplot(task_method_plot, "Gold standard") +
    theme(legend.position = "none", plot.title = element_blank())
  task_method_plot
}
