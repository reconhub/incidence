## TODO: interval that does not divide through onset

context("summary")

## test_that("noop summary", {
##   x <- incidence(c(1, 5, 8, 3, 7, 2, 4, 6, 9, 2))
##   expect_identical(summary(x), x)
## })

## test_that("collapse", {
##   x <- incidence(c(1, 5, 8, 3, 7, 2, 4, 6, 9, 2))
##   y <- summary(x, 2)
##   expect_identical(y$data, x)
##   expect_false(y$rolling)
##   expect_true(all(diff(y$day) == 2L))
##   expect_equal(length(y$day), ceiling(length(x$day) / 2))
##   expect_equal(length(y$day), length(y$cases))
##   expect_equal(sum(y$cases), sum(x$cases))
## })

## test_that("rolling", {
##   x <- incidence(c(1, 5, 8, 3, 7, 2, 4, 6, 9, 2))
##   y <- summary(x, 2, rolling=TRUE)
##   expect_identical(y$data, x)
##   expect_true(y$rolling)
##   expect_true(all(diff(y$day) == 1L))
##   expect_equal(length(y$day), length(x$day) - 1)
##   expect_equal(length(y$day), length(y$cases))
## })
