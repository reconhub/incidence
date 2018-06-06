context("Test cumulate")

test_that("Results as expected", {
  skip_on_cran()

  dat <- as.Date("2016-01-02") + as.integer(c(7,7,7,0,1,2,2,2,3,3,5,7))
  i <- incidence(dat)

  out <- cumulate(i)
  expect_identical(cumsum(i$counts), as.vector(out$counts))
  expect_true(out$cumulative)
  expect_identical(cumulate(-3:10), cumsum(-3:10))
  expect_error(cumulate(out), "x is already a cumulative incidence")

})
