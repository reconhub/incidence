context("Incidence main function")

test_that("construction - default, integer input", {
  skip_on_cran()

  ## USING DAILY INCIDENCE
  set.seed(as.numeric(Sys.time()))
  dat <- as.integer(sample(-3:10, 50, replace = TRUE))
  x <- incidence(dat)

  ## classes
  expect_is(x, "incidence")
  expect_is(x$dates, class(dat))
  expect_is(x$counts, "matrix")

  ## dimensions
  expect_equal(nrow(x$counts), length(x$dates))

  ## results
  expect_false(any(is.na(x$counts)))
  expect_equal(length(x$dates), diff(range(dat)) + 1)
  expect_equal(sum(x$counts), length(dat))
  expect_equal(sum(x$counts), x$n)
  expect_true(all(diff(x$dates) == x$interval))

  ## USING INCIDENCE PER 3 DAYS
  set.seed(as.numeric(Sys.time()))
  dat <- as.integer(sample(-3:10, 50, replace = TRUE))
  x <- incidence(dat, 3)

  ## String numbers can be interpreted as intervals
  expect_identical(x, incidence(dat, "3"))

  ## classes
  expect_is(x, "incidence")
  expect_is(x$dates, class(dat))
  expect_is(x$counts, "matrix")

  ## dimensions
  expect_equal(nrow(x$counts), length(x$dates))

  ## results
  expect_false(any(is.na(x$counts)))
  expect_equal(sum(x$counts), length(dat))
  expect_equal(sum(x$counts), x$n)
  expect_true(all(diff(x$dates) == x$interval))
})

test_that("construction - ISO week", {
  skip_on_cran()

  ## USING WEEKLY INCIDENCE
  # set.seed(as.numeric(Sys.time()))
  # ZNK: Changing this to evaluate the date as an R expression so that the seed
  #      stays constant per day for easier debugging
  set.seed(eval(parse(text = as.character(Sys.Date()))))
  dat <- as.integer(sample(-3:100, 50, replace = TRUE))
  dat.dates <- as.Date("2016-09-20") + dat
  inc.week <- incidence(dat.dates, interval = 7, standard = FALSE)
  inc.isoweek <- incidence(dat.dates, interval = 7)

  ## classes
  expect_is(inc.week, "incidence")
  expect_is(inc.isoweek, "incidence")

  ## dimensions
  expect_equal(setdiff(names(inc.isoweek), names(inc.week)), "isoweeks")
  expect_equal(length(inc.isoweek$isoweeks), length(inc.isoweek$dates))
  expect_equal(nrow(inc.isoweek$counts), length(inc.isoweek$dates))

  ## results
  expect_false(any(is.na(inc.isoweek$counts)))
  expect_equal(sum(inc.isoweek$counts), length(dat))
  expect_equal(sum(inc.isoweek$counts), inc.isoweek$n)
  expect_true(all(diff(inc.isoweek$dates) == inc.isoweek$interval))
})

test_that("construction - numeric input", {
  skip_on_cran()

  ## USING DAILY INCIDENCE
  set.seed(1)
  dat_int <- sample(-3:10, 100, replace = TRUE)
  dat_num <- dat_int + 0.1

  msg <- paste0("Flooring from non-integer date caused approximations:\n",
                "Mean relative difference: 0.0228833")
  expect_warning(incidence(dat_num),
                 msg)

  x_num <- suppressWarnings(incidence(dat_num))
  x_int <- incidence(dat_int)

  ## compare outputs
  expect_equal(x_num, x_int)
  expect_is(x_num$dates, "numeric")
  expect_is(x_int$dates, "integer")
})

test_that("construction - Date input", {
  skip_on_cran()

  ## USING DAILY INCIDENCE
  set.seed(as.numeric(Sys.time()))
  dat <- as.integer(c(-3, sample(-3:100, 50, replace = TRUE)))

  ## note: the choice of dates here makes sure first date is 28 Dec 2015, which
  ## starts an iso week, so that counts will be comparable with/without iso
  dat.dates <- as.Date("2015-12-31") + dat
  x         <- incidence(dat)
  x.dates   <- incidence(dat.dates)
  expect_warning(x.i.trim  <- incidence(dat, first_date = 0),
                 "I removed [0-9]+ observations outside of \\[0, [0-9]+\\]."
                )
  expect_warning(x.d.trim  <- incidence(dat.dates, first_date = "2016-01-01"),
                 "I removed [0-9]+ observations outside of \\[2016-01-01, [-0-9]{10}\\]."
                )
  x.7       <- incidence(dat.dates, 7L, standard = FALSE)
  x.7.iso   <- incidence(dat.dates, "week")
  x.7.week  <- incidence(dat.dates, "week", standard = FALSE)

  ## Here, we can test if starting on a different day gives us expected results
  x.ds       <- incidence(dat.dates + 1L)
  x.7.ds     <- incidence(dat.dates + 1L, 7L, standard = FALSE)
  x.w.ds     <- incidence(dat.dates + 1L, "week", standard = FALSE)
  x.7.ds.iso <- incidence(dat.dates + 1L, 7L)
  x.w.ds.iso <- incidence(dat.dates + 1L, "week")

  ## Testing monthly input
  expect_warning(x.mo.no <- incidence(dat.dates - 28, "month", standard = FALSE),
                 "The first_date \\(2015-11-30\\) represents a day that does not occur in all months.")

  x.mo.iso <- incidence(dat.dates, "month")
  expect_equal(format(x.mo.iso$dates, "%m"), unique(format(sort(dat.dates), "%m")))
  expect_equal(format(x.mo.iso$dates, "%d"), rep("01", 5)) # all starts on first
  expect_equal(x.mo.iso$dates[[1]], as.Date("2015-12-01"))
  expect_equal(sum(x.mo.iso$counts), 51L)

  x.mo <- incidence(dat.dates, "month", standard = FALSE)
  expect_equal(format(x.mo$dates, "%m"), unique(format(sort(dat.dates), "%m"))[-5])
  expect_equal(format(x.mo$dates, "%d"), rep("28", 4)) # all starts on the 28th
  expect_equal(x.mo$dates[[1]], as.Date("2015-12-28"))
  expect_equal(sum(x.mo$counts), 51L)

  ## Testing quarterly input
  expect_warning(x.qu.no <- incidence(dat.dates - 28, "quarter", standard = FALSE),
                 "The first_date \\(2015-11-30\\) represents a day that does not occur in all months.")

  x.qu.iso <- incidence(dat.dates, "quarter")
  expect_equal(x.qu.iso$dates, as.Date(c("2015-10-01", "2016-01-01", "2016-04-01")))
  expect_equal(sum(x.qu.iso$counts), 51L)

  x.qu     <- incidence(dat.dates, "quarter", standard = FALSE)
  expect_equal(x.qu$dates, as.Date(c("2015-12-28", "2016-03-28")))
  expect_equal(sum(x.qu$counts), 51L)

  ## Testing yearly input
  dat.yr <- c(dat.dates,
              sample(dat.dates + 366, replace = TRUE),
              sample(dat.dates + 366 + 365, replace = TRUE)
             )
  x.yr.iso <- incidence(dat.yr, "year")
  x.yr     <- incidence(dat.yr, "year", standard = FALSE)
  expect_warning(x.yr.no  <- incidence(dat.yr, "year", first_date = "2016-02-29"),
                 "The first_date \\(2016-02-29\\) represents a day that does not occur in all years."
                 )
  expect_equal(get_dates(x.yr.iso), as.Date(c("2015-01-01", "2016-01-01", "2017-01-01", "2018-01-01")))
  expect_equal(get_dates(x.yr), as.Date(c("2015-12-28", "2016-12-28", "2017-12-28")))
  expect_equal(sum(x.yr$counts), sum(x.yr.iso$counts))

  ## compare outputs
  expect_equal(x$counts, x.dates$counts)
  expect_is(x$dates, "integer")
  expect_is(x.dates$dates, "Date")
  expect_equal(x.7$counts, x.7.iso$counts)
  expect_equal(x.7.iso$dates, x.7.week$dates)

  # shifting days gives the desired effect
  expect_equal(x.ds$dates[[1]], x.7.ds$dates[[1]])
  expect_equal(x.ds$dates[[1]] - 1L, x.7.ds.iso$dates[[1]])
  expect_identical(x.7.ds.iso$dates, x.w.ds.iso$dates)
  expect_failure({
    expect_identical(x.w.ds$dates, x.w.ds.iso$dates)
  })

  ## Printing will be different with text-based interval
  expect_output(print(x.7), "\\$interval: 7 days")
  expect_output(print(x.7.iso), "\\$interval: 1 week")
})

test_that("construction - POSIXct input", {
  skip_on_cran()

  ## USING DAILY INCIDENCE
  set.seed(as.numeric(Sys.time()))
  dat <- as.integer(sample(-3:100, 50, replace = TRUE))
  dat.dates <- as.Date("2016-09-20") + dat
  dat.pos <- as.POSIXct(dat.dates)
  x.dates <- incidence(dat.dates)
  x.pos <- incidence(dat.pos)

  ## compare outputs
  expect_equal(x.dates$counts, x.pos$counts)
  expect_is(x.dates$dates, "Date")
  expect_is(x.pos$dates, "POSIXct")
})

test_that("corner cases", {
  skip_on_cran()

  expect_error(incidence(integer(0)),
               "At least one \\(non-NA\\) date must be provided")

  expect_error(incidence(numeric(0)),
               "At least one \\(non-NA\\) date must be provided")

  expect_error(incidence(NA),
               "At least one \\(non-NA\\) date must be provided")

  expect_error(incidence(NULL),
               "dates is NULL")

  expect_error(incidence(Inf),
               "At least one \\(non-NA\\) date must be provided")

  expect_error(incidence(1, "grind"),
               "The interval 'grind' is not valid. Please supply an integer.")

  expect_error(incidence(as.Date(Sys.Date()), last_date = "core"),
               "last_date could not be converted to Date")

  expect_error(incidence(1, "week"),
               "The interval 'week' can only be used for Dates")

  expect_error(incidence(as.Date(Sys.Date()), standard = "TRUE"),
               "The argument `standard` must be either `TRUE` or `FALSE`")

})

test_that("Expected values, no group", {
  skip_on_cran()

  expect_true(all(incidence(1:10)$counts == 1L))
  expect_true(all(incidence(sample(1:10))$counts == 1L))

  set.seed(1)
  res1 <- incidence(c(3,2,-1,1,1))
  res2 <- incidence(c(0,0,0))
  res3 <- incidence(sample(1:80, 1000, replace = TRUE))
  res4 <- incidence(as.Date("1984-01-01") + sample(1:100, 200, replace = TRUE))
  res5 <- incidence(c(3,2,-1,1,1), 2L)
  res6 <- incidence(c(0,0,0), 3L)
  res7 <- incidence(sample(1:80, 1000, replace = TRUE), 4L)
  res8 <- incidence(as.Date("1984-01-01") + sample(1:100, 200, replace = TRUE), 12L)

  expect_equal_to_reference(res1, file = "rds/incidence.res1.rds")
  expect_equal_to_reference(res2, file = "rds/incidence.res2.rds")
  expect_equal_to_reference(res3, file = "rds/incidence.res3.rds")
  expect_equal_to_reference(res4, file = "rds/incidence.res4.rds")
  expect_equal_to_reference(res5, file = "rds/incidence.res5.rds")
  expect_equal_to_reference(res6, file = "rds/incidence.res6.rds")
  expect_equal_to_reference(res7, file = "rds/incidence.res7.rds")
  expect_equal_to_reference(res8, file = "rds/incidence.res8.rds")
})

test_that("Expected values, with groups", {
  skip_on_cran()

  dat <- list(
    as.integer(c(3,2,-1,1,1)),
    as.integer(c(0,0,0)),
    as.integer(c(0,1,2,2,3,5,7))
  )

  fac <- list(
    factor(c(1,1,2,2,2)),
    factor(c('a','b','a')),
    factor(c(1, 2, 3, 3, 3, 3, 1))
  )

  res.g.1 <- incidence(dat[[1]], groups = fac[[1]])
  res.g.2 <- incidence(dat[[2]], groups = fac[[2]])
  res.g.3 <- incidence(dat[[3]], groups = fac[[3]])

  expect_equal_to_reference(res.g.1, file = "rds/res.g.1.rds")
  expect_equal_to_reference(res.g.2, file = "rds/res.g.2.rds")
  expect_equal_to_reference(res.g.3, file = "rds/res.g.3.rds")
})

test_that("Printing returns the object", {
  skip_on_cran()

  x <- incidence(as.Date("2001-01-01"))
  y <- incidence(1:2, groups = factor(1:2))
  dat <- as.integer(sample(-3:100, 50, replace = TRUE))
  dat.dates <- as.Date("2016-09-20") + dat
  z <- incidence(dat.dates, interval = 7)
  expect_equal_to_reference(capture.output(print(x)),
                            file = "rds/print1.rds")
  expect_equal_to_reference(capture.output(print(y)),
                            file = "rds/print2.rds")
  expect_equal_to_reference(capture.output(print(z)),
                            file = "rds/print3.rds")
})
