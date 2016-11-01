context("Conversions of incidence objects")


test_that("as.data.frame works", {
  skip_on_cran()
  
  dat <- as.integer(c(0,1,2,2,3,5,7))
  fac <- factor(c(1, 2, 3, 3, 3, 3, 1))
  i <- incidence(dat, groups=fac)
  df <- as.data.frame(i)
  dfl <- as.data.frame(i, long = TRUE)
  df2 <- as.data.frame(incidence(1:2))
  
  expect_equal_to_reference(df, file="rds/df.rds")
  expect_equal_to_reference(dfl, file="rds/dfl.rds")
  expect_equal_to_reference(df2, file="rds/df2.rds")
})
