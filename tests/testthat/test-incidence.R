context("incidence")

test_that("construction - default, integer input", {
    ## USING DAILY INCIDENCE
    set.seed(1)
    dat <- as.integer(sample(-3:10, 50, replace=TRUE))
    x <- incidence(dat)

    ## classes
    expect_is(x, "incidence")
    expect_is(x$dates, class(dat))
    expect_is(x$counts, "matrix")

    ## dimensions
    expect_equal(nrow(x$counts), length(x$dates))

    ## results
    expect_false(any(is.na(x$counts)))
    expect_equal(length(x$dates), diff(range(dat))+1)
    expect_equal(sum(x$counts), length(dat))
    expect_equal(sum(x$counts), x$n)
    expect_true(all(diff(x$dates) == x$interval))

    ## USING INCIDENCE PER 3 DAYS
    set.seed(2)
    dat <- as.integer(sample(-3:10, 50, replace=TRUE))
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





test_that("construction - numeric input", {
    ## USING DAILY INCIDENCE
    set.seed(111)
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
    ## USING DAILY INCIDENCE
    set.seed(123)
    dat <- as.integer(sample(-3:100, 50, replace=TRUE))
    dat.dates <- as.Date("2016-09-20") + dat
    x <- incidence(dat)
    x.dates <- incidence(dat.dates)

    ## compare outputs
    expect_equal(x$counts, x.dates$counts)
    expect_is(x$dates, "integer")
    expect_is(x.dates$dates, "Date")
})




test_that("corner cases", {
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

})




## test_that("negative offsets", {
##   dat <- sample((-5):5, 20, replace=TRUE)
##   x <- incidence(dat)
##   expect_equal(range(x$dates), range(dat))
##   expect_equal(sum(x$counts), length(dat))
## })

## test_that("positive offsets", {
##   dat <- sample(5:15, 20, replace=TRUE)
##   x <- incidence(dat)
##   expect_equal(range(x$dates), range(dat))
##   expect_equal(sum(x$counts), length(dat))
## })

## test_that("from dates", {
##   env <- environment()
##   data("ToyOutbreak", package="OutbreakTools", envir=env)
##   dates <- OutbreakTools::get.data(ToyOutbreak, "DateInfected")

##   x <- incidence(dates)
##   expect_is(x, "incidence")
##   expect_is(x$dates, "Date")
##   expect_equal(range(x$dates), range(dates))

##   for (rolling in c(FALSE, TRUE)) {
##     xx <- summary(x, interval = 2, rolling = rolling)
##     expect_is(xx$dates, "Date")
##     expect_equal(xx$dates[[1]], x$dates[[1]])
##   }
## })

## test_that("to time series", {
##   dat <- c(1, 5, 8, 3, 7, 2, 4, 6, 9, 2)
##   x <- incidence(dat)
##   y <- as.ts(x)
##   expect_is(y, "ts")
## })
