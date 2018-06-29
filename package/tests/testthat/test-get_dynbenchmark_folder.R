context("Testing get_dynbenchmark_folder")

prev_path <- Sys.getenv("DYNBENCHMARK_PATH")
prev_option <- getOption("dynbenchmark_path")

# save old settings
Sys.setenv(DYNBENCHMARK_PATH = "")
options(dynbenchmark_path = NULL)


test_that("get_dynbenchmark_folder works using variables", {
  tmp_path <- tempfile()
  dir.create(tmp_path)

  Sys.setenv(DYNBENCHMARK_PATH = tmp_path)

  file.create(paste0(tmp_path, "/dynbenchmark.Rproj"))

  expect_equal(get_dynbenchmark_folder(), tmp_path)

  Sys.setenv(DYNBENCHMARK_PATH = "")

  expect_error(get_dynbenchmark_folder(), regexp = "dynbenchmark folder could not be found")
})

test_that("get_dynbenchmark_folder works using options", {
  tmp_path <- tempfile()
  dir.create(tmp_path)

  options(dynbenchmark_path = tmp_path)

  file.create(paste0(tmp_path, "/dynbenchmark.Rproj"))

  expect_equal(get_dynbenchmark_folder(), tmp_path)

  options(dynbenchmark_path = NULL)

  expect_error(get_dynbenchmark_folder(), regexp = "dynbenchmark folder could not be found")
})

# revert to old settings
Sys.setenv(DYNBENCHMARK_PATH = prev_path)
options(dynbenchmark_path = prev_option)
