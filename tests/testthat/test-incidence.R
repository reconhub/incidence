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

  expect_message(incidence(c(1,NA,NA)),
                 "2 non-finite values \\(NA, Inf\\) where removed from the data")

})




test_that("Expected values", {
    expect_true(all(incidence(1:10)$counts == 1L))
    expect_true(all(incidence(sample(1:10))$counts == 1L))

    dat <- list(
        as.integer(c(3,2,-1,1,1)),
        as.integer(1),
        as.integer(c(0,0,0)),
        as.integer(c(0,1,2,2,3,5,7))
        )
    exp.res <- list(
        as.integer(c(1,0,2,1,1)),
        as.integer(c(1)),
        as.integer(c(3)),
        as.integer(c(1,1,2,1,0,1,0,1))
        )
    exp.res2 <- list(
        as.integer(c(1,3,1)),
        as.integer(c(1)),
        as.integer(c(3)),
        as.integer(c(2,3,1,1))
        )

    for (i in seq_along(dat)) {
        expect_equal(as.integer(incidence(dat[[i]])$counts), exp.res[[i]])
        expect_equal(as.integer(incidence(dat[[i]], 2L)$counts), exp.res2[[i]])
    }


})



