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

test_that("get_dates works for integers", {
  expect_equal(get_dates(x.1), x.1$dates)
  expect_equal(get_dates(x.1, count_days = TRUE), seq_along(x.1$dates) - 1.0)
  expect_equal(get_dates(x.1, "center"), x.1$dates + 0.5)
  expect_equal(get_dates(x.1, "right"),  x.1$dates + 1)
})

test_that("get_interval works for integer weeks", {
  expect_equal(get_interval(x.7), 7L)
  expect_equal(get_interval(x.7, integer = FALSE), 7L)
})

test_that("get_dates works for integer weeks", {
  expect_equal(get_dates(x.7), x.7$dates)
  expect_equal(get_dates(x.7, count_days = TRUE), 7*(seq_along(x.7$dates) - 1.0))
  expect_equal(get_dates(x.7, "center"), x.7$dates + 3.5)
  expect_equal(get_dates(x.7, "right"),  x.7$dates + 7)
})


test_that("get_interval works for character days", {
  expect_equal(get_interval(x.day), 1L)
  expect_equal(get_interval(x.day, integer = FALSE), "day")
})

test_that("get_dates works for character days", {
  expect_equal(get_dates(x.day), x.day$dates)
  expect_equal(get_dates(x.day, count_days = TRUE), (seq_along(x.day$dates) - 1.0))
  expect_equal(get_dates(x.day, "center"), x.day$dates + 0.5)
  expect_equal(get_dates(x.day, "right"),  x.day$dates + 1.0)
})

test_that("get_interval works for character weeks", {
  expect_equal(get_interval(x.wee), 7L)
  expect_equal(get_interval(x.wee, integer = FALSE), "week")
})

test_that("get_dates works for character weeks", {
  expect_equal(get_dates(x.wee), x.wee$dates)
  expect_equal(get_dates(x.wee, count_days = TRUE), 7*(seq_along(x.wee$dates) - 1.0))
  expect_equal(get_dates(x.wee, "center"), x.wee$dates + 3.5)
  expect_equal(get_dates(x.wee, "right"),  x.wee$dates + 7.0)
})


test_that("get_interval works for character months", {
  expect_equal(get_interval(x.mon), c(31, 28, 31))
  expect_equal(get_interval(x.mon, integer = FALSE), "month")
})

test_that("get_dates works for character months", {
  expect_equal(get_dates(x.mon), x.mon$dates)
  expect_equal(get_dates(x.mon, count_days = TRUE), c(0, 31, 59))
  expect_equal(get_dates(x.mon, "center"), x.mon$dates + c(31, 28, 31)/2)
  expect_equal(get_dates(x.mon, "right"),  x.mon$dates + c(31, 28, 31))
})

test_that("errors happen", {
  xx          <- x.1
  xx$interval <- factor("what")
  expect_error(get_interval(xx), "factor")
})



