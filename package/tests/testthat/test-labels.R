context("Testing plotting_labels")



test_that("Plotting labels works", {
  expect_equal(label_short("one_hundred_and_seven_thousand", width = 10), "One\nhundred\nand seven\nthousand")
  expect_equal(label_long("one_hundred_and_seven_thousand"), "One hundred and seven thousand")
  expect_equal(label_simple_trajectory_types("directed_linear"), "linear")
  expect_equal(label_extrema(c(0, 0.5, 1)), c("0", "", "1"))
})
