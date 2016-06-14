context("incidence")

test_that("construction", {
  dat <- c(1, 5, 8, 3, 7, 2, 4, 6, 9, 2)
  x <- incidence(dat)
  expect_is(x, "incidence")
  expect_equal(length(x$day), length(unique(dat)))
  expect_equal(length(x$cases), length(x$day))
  expect_equal(sum(x$cases), length(dat))
  expect_true(all(diff(x$day) == 1L))
})

test_that("corner cases", {
  skip("not yet implemented")
  expect_error(incidence(integer(0)),
               "Must have at least one record")
  ## Noninteger
  expect_error(incidence(runif(10)),
               "Days must be integer values")

  ## TODO: Missing values (use na.rm perhaps?)

  ## special handling of this one:
  x <- incidence(1)
})

test_that("negative offsets", {
  dat <- sample((-5):5, 20, replace=TRUE)
  x <- incidence(dat)
  expect_equal(range(x$day), range(dat))
  expect_equal(sum(x$cases), length(dat))
})

test_that("positive offsets", {
  dat <- sample(5:15, 20, replace=TRUE)
  x <- incidence(dat)
  expect_equal(range(x$day), range(dat))
  expect_equal(sum(x$cases), length(dat))
})

test_that("from dates", {
  env <- environment()
  data("ToyOutbreak", package="OutbreakTools", envir=env)
  dates <- OutbreakTools::get.data(ToyOutbreak, "DateInfected")

  x <- incidence(dates)
  expect_is(x, "incidence")
  expect_is(x$day, "Date")
  expect_equal(range(x$day), range(dates))

  for (rolling in c(FALSE, TRUE)) {
    xx <- summary(x, interval = 2, rolling = rolling)
    expect_is(xx$day, "Date")
    expect_equal(xx$day[[1]], x$day[[1]])
  }
})

test_that("to time series", {
  dat <- c(1, 5, 8, 3, 7, 2, 4, 6, 9, 2)
  x <- incidence(dat)
  y <- as.ts(x)
  expect_is(y, "ts")
})
