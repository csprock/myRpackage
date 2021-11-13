test_that("Test that make_filename() works as expected", {
  testthat::expect_equal(make_filename("2012"), "accident_2012.csv.bz2")
})
