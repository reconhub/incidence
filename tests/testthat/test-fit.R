context("Test fit functions")

test_that("fit", {
  skip_on_cran()

  dat <- readRDS("data_cache/mfdat.rds")
  sex <- readRDS("data_cache/mfcat.rds")

  i <- incidence(dat, 5L)
  i.sex <- incidence(dat, 5L, groups = sex)
  fit.i <- fit(i)
  expect_warning(fit.i.sex <- fit(i.sex),
                 "3 dates with incidence of 0 ignored for fitting")

  expect_equal_to_reference(fit.i, file = "rds/fit.i.rds")
  expect_equal_to_reference(fit.i.sex, file = "rds/fit.i.sex.rds")
  expect_equal_to_reference(capture.output(fit.i), file = "rds/print.fit.i.rds")
  expect_equal_to_reference(capture.output(fit.i.sex), file = "rds/print.fit.sex.rds")

  ## errors
  x <- incidence(c(1, 0, 0, 0), interval = 7)
  expect_error(fit(x), "Only 1 date with non-zero incidence. Cannot fit model to 1 data point.")
})

test_that("fit_optim_split", {
  skip_on_cran()

  dat <- readRDS("data_cache/mfdat.rds")
  sex <- readRDS("data_cache/mfcat.rds")

  i     <- incidence(dat, 5L)
  i.sex <- incidence(dat, 5L, groups = sex)
  i.fit     <- fit_optim_split(i, plot = FALSE)
  i.fit.sex <- fit_optim_split(i.sex, plot = FALSE)
  expect_equal_to_reference(i.fit,
                            file = "rds/o.fit.i.rds")
  expect_equal_to_reference(i.fit.sex,
                            file = "rds/o.fit.i.sex.rds")

  expect_is(i.fit$df, "data.frame")
  expect_is(i.fit$fit, "incidence_fit_list")

  expect_is(i.fit.sex$df, "data.frame")
  expect_is(i.fit.sex$fit, "incidence_fit_list")

  expect_output(print(i.fit.sex$fit), "<list of incidence_fit objects>")
  expect_output(print(i.fit.sex$fit), "[^N][^A]")
  expect_true(any(is.na(get_info(i.fit.sex$fit, "halving", na.rm = FALSE))))

  ## errors
  expect_error(fit_optim_split(i, window = -1),
               "No date left to try after defining splits to try.")
})

test_that("internals for fitting", {
  skip_on_cran()

  expect_null(extract_info(NULL))
})

test_that("fitting results are the same for incidence fits on Dates and POSIXct", {
  days <- 1:14
  dat_cases <- round(exp(.2*(days)))
  dat_dates_Date <- rep(as.Date(Sys.Date() + days), dat_cases)
  dat_dates_POSIXct <- as.POSIXct(dat_dates_Date)

  iD <- incidence(dat_dates_Date)
  iP <- incidence(dat_dates_POSIXct)

  expect_equal(fit(iP), fit(iD))
})
