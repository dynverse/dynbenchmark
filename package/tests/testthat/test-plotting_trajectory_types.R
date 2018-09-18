context("Test plotting trajectory types")


test_that("test plot_trajectory_type", {
  plot <- ggplot() +
    theme_void()

  new_plot <- plot %>%
    plot_trajectory_types(trajectory_types$id, ymin = seq_along(trajectory_types$id), ymax = seq_along(trajectory_types$id) + 1, size = 1.5)

  testthat::expect_true(is.ggplot(new_plot))
})
