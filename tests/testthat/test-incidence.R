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
  set.seed(as.numeric(Sys.time()))
  dat <- as.integer(sample(-3:100, 50, replace = TRUE))
  dat.dates <- as.Date("2016-09-20") + dat
  inc.week <- incidence(dat.dates, interval = 7, iso_week = FALSE)
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
  set.seed(as.numeric(Sys.time()))
  dat.num <- runif(50, -3, 10)
  dat.int <- as.integer(floor(dat.num))
  expect_message(incidence(dat.num),
                 "Dates stored as decimal numbers were floored")
  x.num <- incidence(dat.num)
  x.int <- incidence(dat.int)

  ## compare outputs
  expect_equal(x.num, x.int)
  expect_is(x.num$dates, "numeric")
  expect_is(x.int$dates, "integer")
})


test_that("construction - Date input", {
  skip_on_cran()

  ## USING DAILY INCIDENCE
  set.seed(as.numeric(Sys.time()))
<<<<<<< HEAD
  dat <- as.integer(sample(-3:100, 50, replace = TRUE))
  dat.dates <- as.Date("2016-09-20") + dat
  x <- incidence(dat)
  x.dates <- incidence(dat.dates)

=======
  dat <- as.integer(c(-3, sample(-3:100, 50, replace=TRUE)))

  ## note: the choice of dates here makes sure first date is 28 Dec 2015, which
  ## starts an iso week, so that counts will be comparable with/without iso
  
  dat.dates <- as.Date("2015-12-31") + dat
  x <- incidence(dat)
  x.dates <- incidence(dat.dates)
  x.7 <- incidence(dat.dates, 7L, iso_week = FALSE)
  x.7.iso <- incidence(dat.dates, 7L)
  
>>>>>>> reconhub/isoweek
  ## compare outputs
  expect_equal(x$counts, x.dates$counts)
  expect_is(x$dates, "integer")
  expect_is(x.dates$dates, "Date")
  expect_equal(x.7$counts, x.7.iso$counts)
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
               "At least one \\(non-NA\\) date must be provided")

  expect_error(incidence(Inf),
               "At least one \\(non-NA\\) date must be provided")

  expect_message(incidence(c(1,NA,NA)),
                 "2 non-finite values \\(NA, Inf\\) where removed from the data")
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
  expect_equal_to_reference(capture.output(print(x)),
                            file = "rds/print1.rds")
  expect_equal_to_reference(capture.output(print(y)),
                            file = "rds/print2.rds")
})
