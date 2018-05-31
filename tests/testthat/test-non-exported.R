context("Non-exported functions")

test_that("check_dates works", {
  msg <- "NA detected in the dates"
  expect_error(check_dates(c(1,2,NA), TRUE), msg)

  msg <- paste0("Flooring from non-integer date caused approximations:\n",
                "Mean relative difference: 0.1")
  expect_warning(check_dates(1.1), msg)

  msg <- paste0("Input could not be converted to date. Accepted formats are:\n",
                "Date, POSIXct, integer, numeric, character")
  expect_error(check_dates(factor("2001-01-01")), msg)

  x <- list(1L,
            as.POSIXct("2001-01-01"),
            as.Date("2001-01-01") + 1:10,
            1.0,
            100:1)
  for (e in x) {
    expect_equal(e, check_dates(e))
  }

  expect_equal(check_dates("2001-01-01"), as.Date("2001-01-01"))
})

test_that("check_interval", {
  skip_on_cran()

  expect_error(check_interval(),
               "Interval is missing or NULL")
  expect_error(check_interval(NULL),
               "Interval is missing or NULL")
  expect_error(check_interval(1:2),
               "Exactly one value should be provided as interval \\(2 provided\\)")
  expect_error(check_interval(integer(0)),
               "Exactly one value should be provided as interval \\(0 provided\\)")
  expect_error(check_interval(NA),
               "Interval is not finite")
  expect_error(check_interval(-Inf),
               "Interval is not finite")
  expect_error(check_interval(.1),
               "Interval must be at least 1 \\(input: 0.100; after rounding: 0\\)")
  expect_equal(check_interval(1), 1)
  expect_equal(check_interval(2.4), 2)
  expect_equal(check_interval(2.7), 3)
})

test_that("check_groups", {
  skip_on_cran()

  expect_is(check_groups(1, 1, FALSE), "factor")
  expect_error(check_groups(1, 1:2, FALSE),
               "'x' does not have the same length as dates \\(1 vs 2\\)")
  expect_equal(check_groups(NULL, NULL, FALSE), NULL)
  expect_equal(check_groups(c(1, 1, 2, NA, 2), 1:5, na_as_group = FALSE),
               factor(c(1, 1, 2, NA, 2)))
  expect_equal(check_groups(c(1,1,2,NA,2), 1:5, na_as_group = TRUE),
               factor(c(1, 1, 2, "NA", 2)))
})



test_that("make_iso_weeks_breaks works", {
  ## uses references from:
  ## https://en.wikipedia.org/wiki/ISO_week_date
  ref <- as.Date("2018-05-01")
  dates <- ref + (-1:20)

  out <- make_iso_weeks_breaks(dates)
  expect_equal_to_reference(out, file = "rds/ref_date_breaks_iso.rds")
})
