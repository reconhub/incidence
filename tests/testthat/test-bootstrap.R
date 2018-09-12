context("Bootstrapping incidence")

set.seed(1)
dates <- as.integer(sample(-3:100, 50, replace = TRUE))
DATES <- as.Date("2016-09-20") + dates
groups <- sample(c("toto", "tata"), 50, replace = TRUE)


test_that("Bootstrap needs an incidence object", {
  expect_error(bootstrap(DATES), "x is not an incidence object")
})

test_that("estimate_peak needs an incidence object", {
  expect_error(estimate_peak(DATES), "x is not an incidence object")
})

test_that("Bootstrap incidence with groups", {
  skip_on_cran()

  x <- incidence(DATES, 3, groups = groups)
  y <- bootstrap(x)
  z <- bootstrap(x, TRUE)
  expect_identical(colSums(x$counts), colSums(y$counts))
  expect_identical(colSums(x$counts), colSums(z$counts))
  expect_identical(colnames(x$counts), colnames(y$counts))
  expect_identical(colnames(x$counts), colnames(z$counts))
  expect_identical(x$interval, y$interval)
  expect_identical(x$interval, z$interval)

})


context("Mountain Climbing")

test_that("find_peak can find the peak", {
  skip_on_cran()


  x <- incidence(DATES, 3, groups = groups)
 
  expect_error(find_peak(1:10), "x is not an incidence object")
  expect_message(p1 <- find_peak(x), "'x' is stratified by groups\npooling groups before finding peaks")
  expect_length(p1, 1L)
  expect_named(find_peak(x, pool = FALSE), c("tata", "toto"))
})


test_that("estimate_peak can roughly estimate it", {
  x <- incidence(DATES, 3, groups = groups)
  y <- incidence(dates, 3)
  expect_message(e1 <- estimate_peak(x), "'x' is stratified by groups\npooling groups before finding peaks")
  e2 <- estimate_peak(y)
  expect_named(e1, c("observed", "estimated", "ci", "peaks"))
  expect_named(e2, c("observed", "estimated", "ci", "peaks"))
  ## The observed is identical to find_peak
  expect_identical(e1$observed, find_peak(pool(x)))
  expect_identical(e2$observed, find_peak(pool(y)))
  ## The number of peaks defaults to 100
  expect_length(e1$peaks, 100)
  expect_length(e2$peaks, 100)
  ## The observed falls within the confidence interval
  expect_gt(as.integer(e1$observed), as.integer(e1$ci[1]))
  expect_lt(as.integer(e1$observed), as.integer(e1$ci[2]))
  expect_gt(as.integer(e2$observed), as.integer(e2$ci[1]))
  expect_lt(as.integer(e2$observed), as.integer(e2$ci[2]))
})
