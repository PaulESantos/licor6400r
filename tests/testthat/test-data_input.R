context("Tests file reading and validation.")

test_that("read_6400 creates a licor object", {
  #Test reading in with an internal file
  x <- read_6400(system.file("extdata",
                             "almont_06072021_day.txt",
                             package = "licor6400",
                             mustWork = TRUE))
  expect_equal(class(x), c("tbl_df", "tbl", "data.frame"))

})

