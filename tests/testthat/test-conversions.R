context("Conversions of incidence objects")


test_that("as.data.frame works", {
  skip_on_cran()

  dat <- as.integer(c(0,1,2,2,3,5,7))
  dat2 <- as.Date("2016-01-02") + dat
  fac <- factor(c(1, 2, 3, 3, 3, 3, 1))
  i <- incidence(dat, groups = fac)
  i.7 <- incidence(dat2, 7L, iso_weeks = TRUE)
  i.7.group <- incidence(dat2, 7L, iso_weeks = TRUE, groups = fac)
  df <- as.data.frame(i)
  dfl <- as.data.frame(i, long = TRUE)
  df2 <- as.data.frame(incidence(1:2))
  df3 <- as.data.frame(i.7)
  df4 <- as.data.frame(i.7, long = TRUE)
  df5 <- as.data.frame(i.7.group)
  df6 <- as.data.frame(i.7.group, long = TRUE)

  expect_equal_to_reference(df, file = "rds/df.rds")
  expect_equal_to_reference(dfl, file = "rds/dfl.rds")
  expect_equal_to_reference(df2, file = "rds/df2.rds")
  expect_equal_to_reference(df3, file = "rds/df3.rds")
  expect_equal(df3, df4)
  expect_equal_to_reference(df5, file = "rds/df5.rds")
  expect_equal_to_reference(df6, file = "rds/df6.rds")
})






test_that("as.incidence works", {
  skip_on_cran()

  dates_int <- sample(1:15, 100, replace = TRUE)
  dates <- as.Date("2017-04-01") + dates_int
  groups <- sample(letters[1:3], 100, replace = TRUE)

  i1 <- incidence(dates, interval = 2)
  i2 <- incidence(dates_int)
  i3 <- incidence(dates, interval = 7, groups = groups)

  expect_equal(as.incidence(i1$counts, i1$dates), i1)
  expect_equal(as.incidence(as.vector(i1$counts), i1$dates), i1)
  expect_equal(as.incidence(i2$counts, i2$dates), i2)
  expect_equal(as.incidence(i3$counts, i3$dates), i3)

  msg <- "Interval needs to be specified if there is only one date."
  expect_error(as.incidence(i3$counts[1,,drop=FALSE], i3$dates[1]),
               msg)

  msg <- "Columns should be named to label groups."
  expect_error(as.incidence(unname(i3$counts), i3$dates),
               msg)

})
