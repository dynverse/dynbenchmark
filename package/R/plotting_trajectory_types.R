#' Plot a trajectory type
#'
#' @param plot Existing plot
#' @param trajectory_types One or more trajectory types
#' @param xmins Minimal x bound
#' @param xmaxs Maximal y bound
#' @param ymins Minimal y bound
#' @param ymaxs Maximal y bound
#' @param size Size of the arrows and dots
#'
#' @export
plot_trajectory_types <- function(plot = ggplot() + theme_void(), trajectory_types, xmins = 0, xmaxs = 1, ymins = 0, ymaxs = 1, size = 4) {
  # make sure all variables have the same length
  if (length(xmins) == 1) xmins <- rep(xmins, length(trajectory_types))
  if (length(xmaxs) == 1) xmaxs <- rep(xmaxs, length(trajectory_types))
  if (length(ymins) == 1) ymins <- rep(ymins, length(trajectory_types))
  if (length(ymaxs) == 1) ymaxs <- rep(ymaxs, length(trajectory_types))

  testthat::expect_true(length(unique(c(length(trajectory_types), length(xmins), length(xmaxs), length(ymins), length(ymaxs)))) == 1)

  # loop over every trajectory type and plot
  for (i in seq_along(trajectory_types)) {
    trajectory_type <- trajectory_types[i]

    trajectory_type_info <- dynutils::extract_row_to_list(dynwrap::trajectory_types, which(dynwrap::trajectory_types$id == trajectory_type))

    xmin <- xmins[i]
    xmax <- xmaxs[i]
    ymin <- ymins[i]
    ymax <- ymaxs[i]

    network <- trajectory_type_info$example_network
    nodes <- trajectory_type_info$example_nodes %>% mutate(id = row_number())

    # change positions of nodes based on bounds
    nodes$x <- nodes$x * (xmax - xmin) / 5 + xmin
    nodes$y <- nodes$y * (ymax - ymin) / 6 + ymin

    network_positions <- network %>%
      left_join(nodes, c("from"="id")) %>%
      left_join(nodes %>% rename_all(~paste0(., "end")), c("to"="idend")) %>%
      mutate(xmid = x + (xend - x) / 1.5, ymid = y + (yend - y) / 1.5)

    plot <- plot +
      geom_segment(
        aes(x = x, y = y, xend = xend, yend = yend),
        data = network_positions,
        size = size / 3
      ) +
      geom_segment(
        aes(x = x, y = y, xend = xmid, yend = ymid),
        data = network_positions,
        arrow = arrow(type = "closed", length = unit(0.04 * size, "inches"))
      ) +
      geom_point(aes(x, y), data = nodes, size = size, color = trajectory_type_info$colour)
  }

  plot
}
