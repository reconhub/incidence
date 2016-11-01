context("Pool")


test_that("Pool works", {
  skip_on_cran()
  
  expect_error(pool(1),
               "x should be an 'incidence' object \\(its class is: numeric\\)")
  expect_identical(incidence(1L), pool(incidence(1L)))
  
  dat <- as.integer(c(0,1,2,2,3,5,7))
  fac <- factor(c(1, 2, 3, 3, 3, 3, 1))
  expect_identical(pool(incidence(dat, groups=fac)), incidence(dat))
  
})
