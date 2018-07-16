context("Accessor tests")

set.seed(999)
int   <- sample(-3:50, 100, replace = TRUE)
dat   <- as.Date("2018-01-31") + int
x.1   <- incidence(int)
x.7   <- incidence(int, 7L)
x.day <- incidence(dat, "day")
x.wee <- incidence(dat, "week")
x.mon <- incidence(dat, "month")


test_that("get_interval works for integers", {
  expect_equal(get_interval(x.1), 1L)
  expect_equal(get_interval(x.1, integer = FALSE), 1L)
})


test_that("get_interval works for integer weeks", {
  expect_equal(get_interval(x.7), 7L)
  expect_equal(get_interval(x.7, integer = FALSE), 7L)
})


test_that("get_interval works for character days", {
  expect_equal(get_interval(x.day), 1L)
  expect_equal(get_interval(x.day, integer = FALSE), "day")
})


test_that("get_interval works for x", {
  expect_equal(get_interval(x.wee), 7L)
  expect_equal(get_interval(x.wee, integer = FALSE), "week")
})


test_that("get_interval works for x", {
  expect_equal(get_interval(x.mon), c(31, 28, 31))
  expect_equal(get_interval(x.mon, integer = FALSE), "month")
})

test_that("errors happen", {
  xx          <- x.1
  xx$interval <- factor("what")
  expect_error(get_interval(xx), "factor")
})



