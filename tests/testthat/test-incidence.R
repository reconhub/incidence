context("Incidence main function")

# setting up the data --------------------------------------------------
the_seed <- eval(parse(text = as.character(Sys.Date())))

# Date incidence      --------------------------------------------------
# note: the choice of dates here makes sure first date is 28 Dec 2015, which
# starts an iso week, so that counts will be comparable with/without iso.
# This also ensures that the last date is 2016-04-04 so that there are 15 weeks
# represented here. 
set.seed(the_seed)
dat       <- as.integer(c(-3, sample(-3:100, 49, replace = TRUE), 100))
dat_dates <- as.Date("2015-12-31") + dat

test_that("construction - default, integer input", {


  ## USING DAILY INCIDENCE
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


  ## USING WEEKLY INCIDENCE
  inc.week    <- incidence(dat_dates, interval = 7, standard = FALSE)
  inc.isoweek <- incidence(dat_dates, interval = 7)

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

  
  ## USING DAILY INCIDENCE
  dat_int <- c(0L, 2L, 5L, 9L, -1L, 9L, 10L, 6L, 5L, -3L, -1L, -1L, 6L, 2L, 7L,
               3L, 7L, 10L, 2L, 7L, 10L, -1L, 6L, -2L, 0L, 2L, -3L, 2L, 9L, 1L,
               3L, 5L, 3L, -1L, 8L, 6L, 8L, -2L, 7L, 2L, 8L, 6L, 7L, 4L, 4L,
               8L, -3L, 3L, 7L, 6L, 3L, 9L, 3L, 0L, -3L, -2L, 1L, 4L, 6L, 2L,
               9L, 1L, 3L, 1L, 6L, 0L, 3L, 7L, -2L, 9L, 1L, 8L, 1L, 1L, 3L, 9L,
               9L, 2L, 7L, 10L, 3L, 6L, 2L, 1L, 7L, -1L, 6L, -2L, 0L, -1L, 0L,
               -3L, 5L, 9L, 7L, 8L, 3L, 2L, 8L, 5L)

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


  x         <- incidence(dat)
  x.dates   <- incidence(dat_dates)
  expect_message(x.i.trim  <- incidence(dat, first_date = 0),
                 "[0-9]+ observations outside of \\[0, [0-9]+\\] were removed."
  )
  expect_warning({
  expect_message({
    x.d.trim  <- incidence(dat_dates, first_date = "2016-01-01")
  }, "[0-9]+ observations outside of \\[2016-01-01, [-0-9]{10}\\] were removed.")
  }, "options\\(incidence.warn.first_date = FALSE\\)")
  expect_message({
  expect_failure(expect_warning({
    x.d.trim  <- incidence(dat_dates, first_date = "2016-01-01")
  }, "options\\(incidence.warn.first_date = FALSE\\)"))
  }, "[0-9]+ observations outside of \\[2016-01-01, [-0-9]{10}\\] were removed.")
  x.7       <- incidence(dat_dates, 7L, standard = FALSE)
  x.7.iso   <- incidence(dat_dates, "week")
  x.7.week  <- incidence(dat_dates, "week", standard = FALSE)
  expect_warning(x.7.week2  <- incidence(dat_dates, "week", iso_week = FALSE),
                 "`iso_week` has been deprecated")
  # iso_week can reset standard, but is given a warning
  expect_identical(x.7.week2, x.7.week)

  ## Here, we can test if starting on a different day gives us expected results
  x.ds       <- incidence(dat_dates + 1L)
  x.7.ds     <- incidence(dat_dates + 1L, 7L, standard = FALSE)
  x.w.ds     <- incidence(dat_dates + 1L, "week", standard = FALSE)
  x.7.ds.iso <- incidence(dat_dates + 1L, 7L)
  x.w.ds.iso <- incidence(dat_dates + 1L, "week")

  ## Testing monthly input
  expect_warning(x.mo.no <- incidence(dat_dates - 28, "month", standard = FALSE),
                 "The first_date \\(2015-11-30\\) represents a day that does not occur in all months.")

  x.mo.iso <- incidence(dat_dates, "month")
  expect_equal(format(x.mo.iso$dates, "%m"), unique(format(sort(dat_dates), "%m")))
  expect_equal(format(x.mo.iso$dates, "%d"), rep("01", 5)) # all starts on first
  expect_equal(x.mo.iso$dates[[1]], as.Date("2015-12-01"))
  expect_equal(sum(x.mo.iso$counts), 51L)

  x.mo <- incidence(dat_dates, "month", standard = FALSE)
  expect_equal(format(x.mo$dates, "%m"), unique(format(sort(dat_dates), "%m"))[-5])
  expect_equal(format(x.mo$dates, "%d"), rep("28", 4)) # all starts on the 28th
  expect_equal(x.mo$dates[[1]], as.Date("2015-12-28"))
  expect_equal(sum(x.mo$counts), 51L)

  ## Testing quarterly input
  expect_warning(x.qu.no <- incidence(dat_dates - 28, "quarter", standard = FALSE),
                 "The first_date \\(2015-11-30\\) represents a day that does not occur in all months.")

  x.qu.iso <- incidence(dat_dates, "quarter")
  expect_equal(x.qu.iso$dates, as.Date(c("2015-10-01", "2016-01-01", "2016-04-01")))
  expect_equal(sum(x.qu.iso$counts), 51L)

  x.qu     <- incidence(dat_dates, "quarter", standard = FALSE)
  expect_equal(x.qu$dates, as.Date(c("2015-12-28", "2016-03-28")))
  expect_equal(sum(x.qu$counts), 51L)

  ## Testing yearly input
  dat.yr <- c(dat_dates,
              sample(dat_dates + 366, replace = TRUE),
              sample(dat_dates + 366 + 365, replace = TRUE)
  )
  x.yr.iso <- incidence(dat.yr, "year")
  x.yr     <- incidence(dat.yr, "year", standard = FALSE)
  expect_warning(x.yr.no  <- incidence(dat.yr, "year", first_date = as.Date("2016-02-29"), standard = FALSE),
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


  ## USING DAILY INCIDENCE
  dat.pos <- as.POSIXct(dat_dates)
  x.dates <- incidence(dat_dates)
  x.pos <- incidence(dat.pos)

  ## compare outputs
  expect_equal(x.dates$counts, x.pos$counts)
  expect_is(x.dates$dates, "Date")
  expect_is(x.pos$dates, "POSIXct")
})

test_that("construction - character input", {
  dats <- Sys.Date() + sample(-100:100, 5)
  datc <- as.character(dats)

  i.date <- incidence(dats)
  i.char <- incidence(datc)
  i.chaw <- incidence(paste(datc, "   "))
  expect_message(i.cham <- incidence(c(datc, NA, NA)), "2 missing observations were removed.")
  expect_is(i.date, "incidence")
  expect_identical(i.date, i.char)  
  expect_identical(i.date, i.chaw)  
  expect_identical(i.date, i.cham)  
})


test_that("corner cases", {


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
               "last_date \\(core\\) could not be converted to Date. Dates must be in ISO 8601 standard format \\(yyyy-mm-dd\\)")

  expect_error(incidence(1, "week"),
               "The interval 'week' can only be used for Dates")

  expect_error(incidence(as.Date(Sys.Date()), standard = "TRUE"),
               "The argument `standard` must be either `TRUE` or `FALSE`")

  expect_error(incidence(sample(10), intrval = 2),
               "intrval : interval")

  expect_error(incidence(1, were = "wolf"), "were")


  expect_warning(incidence(c(dat_dates, as.Date("1900-01-01"))),
                 "greater than 18262 days \\[1900-01-01 to"
  )
  
  msg <- 'Not all dates are in ISO 8601 standard format \\(yyyy-mm-dd\\). The first incorrect date is'
  expect_error(incidence('daldkadl'), paste(msg, "daldkadl"))
  
  dats <- as.character(Sys.Date() + sample(-10:10, 5))
  dats[3] <- "1Q84-04-15" 
  expect_error(incidence(dats), paste(msg, "1Q84-04-15"))

  dats[3] <- "2018-69-11"
  expect_error(incidence(dats), paste(msg, "2018-69-11"))

  dats[3] <- "01-01-11"
  expect_error(incidence(dats), paste(msg, "01-01-11"))

  dats[3] <- "01-Apr-11"
  expect_error(incidence(dats), paste(msg, "01-Apr-11"))

  msg <- paste0("Input could not be converted to date. Accepted formats are:\n",
                "Date, POSIXct, integer, numeric, character")
  expect_error(incidence(factor("2001-01-01")), msg)
})

test_that("incidence constructor can handle missing data", {
  miss_dat <- dat
  miss_dat[5] <- NA
  expect_message(incidence(miss_dat), "1 missing observations were removed.")
})

test_that("incidence constructor can handle data out of range with groups", {
  set.seed(the_seed)
  g <- sample(letters[1:2], length(dat), replace = TRUE)
  expect_message(incidence(dat, first_date = 0, groups = g),
                 "[0-9]+ observations outside of \\[0, [0-9]+\\] were removed."
  )
})

test_that("Expected values, no group", {


  expect_true(all(incidence(1:10)$counts == 1L))
  expect_true(all(incidence(sample(1:10))$counts == 1L))

  # set.seed(1)
  res1 <- incidence(c(3,2,-1,1,1))
  res2 <- incidence(c(0,0,0))
  # res3 <- incidence(sample(1:80, 1000, replace = TRUE))
  # res4 <- incidence(as.Date("1984-01-01") + sample(1:100, 200, replace = TRUE))
  res5 <- incidence(c(3,2,-1,1,1), 2L)
  res6 <- incidence(c(0,0,0), 3L)
  # res7 <- incidence(sample(1:80, 1000, replace = TRUE), 4L)
  # res8 <- incidence(as.Date("1984-01-01") + sample(1:100, 200, replace = TRUE), 12L)

  expect_equal_to_reference(res1, file = "rds/incidence.res1.rds")
  expect_equal_to_reference(res2, file = "rds/incidence.res2.rds")
  # expect_equal_to_reference(res3, file = "rds/incidence.res3.rds")
  # expect_equal_to_reference(res4, file = "rds/incidence.res4.rds")
  expect_equal_to_reference(res5, file = "rds/incidence.res5.rds")
  expect_equal_to_reference(res6, file = "rds/incidence.res6.rds")
  # expect_equal_to_reference(res7, file = "rds/incidence.res7.rds")
  # expect_equal_to_reference(res8, file = "rds/incidence.res8.rds")
})

test_that("Expected values, with groups", {


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

test_that("user-defined group levels are preserved", {
  g <- sample(LETTERS[1:5], 100, replace = TRUE)
  g <- factor(g, levels = LETTERS[5:1])
  i <- incidence(rpois(100, 10), groups = g)
  expect_identical(group_names(i), levels(g))
  i.df <- as.data.frame(i, long = TRUE)
  expect_identical(levels(i.df$groups), levels(g))
})

test_that("Printing returns the object", {


  x <- incidence("2001-01-01")
  y <- incidence(1:2, groups = factor(1:2))
  z <- incidence(dat_dates, interval = 7)
  expect_equal_to_reference(capture.output(print(x)),
                            file = "rds/print1.rds")
  expect_equal_to_reference(capture.output(print(y)),
                            file = "rds/print2.rds")
  expect_equal_to_reference(capture.output(print(z)),
                            file = "rds/print3.rds")
})

