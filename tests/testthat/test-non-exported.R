context("Non-exported functions")

test_that("check_interval", {
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
    expect_is(check_groups(1, 1, FALSE), "factor")
    expect_error(check_groups(1, 1:2, FALSE),
                  "'groups' does not have the same length as dates \\(1 vs 2\\)")
    expect_equal(check_groups(NULL,NULL,FALSE), NULL)
    expect_equal(check_groups(c(1,1,2,NA,2), 1:5, na_as_group=FALSE),
                 factor(c(1,1,2,NA,2)))
    expect_equal(check_groups(c(1,1,2,NA,2), 1:5, na_as_group=TRUE),
                 factor(c(1,1,2,"NA",2)))
})

