context("Bootstrapping incidence")


test_that("Bootstrap incidence with groups", {
  skip_on_cran()

  dates <- as.integer(sample(-3:100, 50, replace = TRUE))
  dates <- as.Date("2016-09-20") + dates
  groups <- sample(c("toto", "tata"), 50, replace = TRUE)

  x <- incidence(dates, 3, groups = groups)
  y <- bootstrap(x)
  z <- bootstrap(x, TRUE)
  expect_identical(colSums(x$counts), colSums(y$counts))
  expect_identical(colSums(x$counts), colSums(z$counts))
  expect_identical(colnames(x$counts), colnames(y$counts))
  expect_identical(colnames(x$counts), colnames(z$counts))
  expect_identical(x$interval, y$interval)
  expect_identical(x$interval, z$interval)

})
