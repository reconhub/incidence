
context("Test fit functions")


test_that("fit", {
  skip_on_cran()

  set.seed(1)
  dat <- c(sample(1:50, 200, replace = TRUE, prob = 1 + exp(1:50 * 0.1)),
           sample(51:100, 200, replace = TRUE, prob = rev(1 + exp(1:50 * 0.1))))
  sex <- sample(c("female", "male"), 400, replace = TRUE)

  i <- incidence(dat, 5L)
  i.sex <- incidence(dat, 5L, groups = sex)
  fit.i <- fit(i)
  expect_warning(fit.i.sex <- fit(i.sex),
                 "3 dates with incidence of 0 ignored for fitting")

  expect_equal_to_reference(fit.i, file="rds/fit.i.rds")
  expect_equal_to_reference(fit.i.sex, file="rds/fit.i.sex.rds")
  expect_equal_to_reference(capture.output(fit.i), file="rds/print.fit.i.rds")
  expect_equal_to_reference(capture.output(fit.i.sex), file="rds/print.fit.sex.rds")

})





test_that("fit_optim_split", {
  skip_on_cran()

  set.seed(1)
  dat <- c(sample(1:50, 200, replace = TRUE, prob = 1 + exp(1:50 * 0.1)),
           sample(51:100, 200, replace = TRUE, prob = rev(1 + exp(1:50 * 0.1))))
  sex <- sample(c("female", "male"), 400, replace = TRUE)

  i <- incidence(dat, 5L)
  i.sex <- incidence(dat, 5L, groups = sex)

  expect_equal_to_reference(fit_optim_split(i), file="rds/o.fit.i.rds")
  expect_equal_to_reference(fit_optim_split(i.sex), file="rds/o.fit.i.sex.rds")

  ## errors
  expect_error(fit_optim_split(i, window = -1),
               "No date left to try after defining splits to try.")

})




test_that("internals for fitting", {
  skip_on_cran()

  expect_null(extract_info(NULL))
})



