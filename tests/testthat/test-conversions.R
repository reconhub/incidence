context("Conversions of incidence objects")


test_that("as.data.frame works", {
  skip_on_cran()
  
  dat <- as.integer(c(0,1,2,2,3,5,7))
  dat2 <- as.Date("2016-01-02") + dat
  fac <- factor(c(1, 2, 3, 3, 3, 3, 1))
  i <- incidence(dat, groups=fac)
  i.7 <- incidence(dat2, 7L, iso_weeks = TRUE)
  df <- as.data.frame(i)
  dfl <- as.data.frame(i, long = TRUE)
  df2 <- as.data.frame(incidence(1:2))
  df3 <- as.data.frame(i.7)
  df4 <- as.data.frame(i.7, long = TRUE)
  
  expect_equal_to_reference(df, file="rds/df.rds")
  expect_equal_to_reference(dfl, file="rds/dfl.rds")
  expect_equal_to_reference(df2, file="rds/df2.rds")
  expect_equal_to_reference(df3, file="rds/df3.rds")
  expect_equal(df3, df4)
  
})
