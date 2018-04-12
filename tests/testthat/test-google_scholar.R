context("Testing google_scholar")

test_that("Testing google_scholar", {
  skip_on_travis()

  sch_out <- google_scholar("6708368002535418936")
  expect_equal(colnames(sch_out), c("cluster_id", "title", "url", "num_citations", "web_of_science"))

  sch_out2 <- google_scholar_num_citations("14546114750226917320")
  expect_gte(sch_out2, 0)
})
