context("Incidence main function")

test_that("construction - default, integer input", {
    ## USING DAILY INCIDENCE
    set.seed(as.numeric(Sys.time()))
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
    set.seed(as.numeric(Sys.time()))
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
    ## USING DAILY INCIDENCE
    set.seed(as.numeric(Sys.time()))
    dat <- as.integer(sample(-3:100, 50, replace=TRUE))
    dat.dates <- as.Date("2016-09-20") + dat
    x <- incidence(dat)
    x.dates <- incidence(dat.dates)

    ## compare outputs
    expect_equal(x$counts, x.dates$counts)
    expect_is(x$dates, "integer")
    expect_is(x.dates$dates, "Date")
})





test_that("construction - POSIXct input", {
    ## USING DAILY INCIDENCE
    set.seed(as.numeric(Sys.time()))
    dat <- as.integer(sample(-3:100, 50, replace=TRUE))
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
    exp.res <- list(
        structure(c(0L, 0L, 0L, 1L, 1L, 1L, 0L, 2L, 0L, 0L),
                  .Dim = c(5L, 2L),
                  .Dimnames = list(NULL, c("1", "2"))),
        structure(c(2L, 1L),
                  .Dim = 1:2,
                  .Dimnames = list(NULL, c("a", "b"))),
        structure(c(1L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 0L, 1L, 0L, 0L, 0L,
                    0L, 0L, 0L, 0L, 0L, 2L, 1L, 0L, 1L, 0L, 0L),
                  .Dim = c(8L, 3L),
                  .Dimnames = list(NULL, c("1", "2", "3")))
        )
    exp.res3 <- list(
        structure(c(0L, 2L, 3L, 0L), .Dim = c(2L, 2L),
                  .Dimnames = list(NULL, c("1", "2"))),
        structure(c(2L, 1L), .Dim = 1:2,
                  .Dimnames = list(NULL, c("a", "b"))),
        structure(c(1L, 0L, 1L, 1L, 0L, 0L, 2L, 2L, 0L),
                  .Dim = c(3L, 3L),
                  .Dimnames = list(NULL, c("1", "2", "3")))
        )

    for (i in seq_along(dat)) {
        expect_equal(incidence(dat[[i]], groups=fac[[i]])$counts,
                     exp.res[[i]])
        expect_equal(incidence(dat[[i]], 3L, groups=fac[[i]])$counts,
                     exp.res3[[i]])
    }

})





test_that("Printing returns the object", {
    x <- incidence(0)
    y <- incidence(1:2, groups=factor(1:2))
    expect_identical(invisible(print(x)), x)
    expect_identical(invisible(print(y)), y)
})
