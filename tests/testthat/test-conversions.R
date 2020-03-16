context("Conversions of incidence objects")

test_that("as.data.frame works", {
  skip_on_cran()
  # skip("re-write with aweek in place")

  dat <- as.integer(c(0,1,2,2,3,5,7))
  dat2 <- as.Date("2016-01-02") + dat
  fac <- factor(c(1, 2, 3, 3, 3, 3, 1))
  one_group <- rep("a", 7)


  i_group_df <- data.frame(
         dates = c(0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L),
            "1" = c(1L, 0L, 0L, 0L, 0L, 0L, 0L, 1L),
            "2" = c(0L, 1L, 0L, 0L, 0L, 0L, 0L, 0L),
            "3" = c(0L, 0L, 2L, 1L, 0L, 1L, 0L, 0L),
            check.names = FALSE, stringsAsFactors = TRUE
  )
  # one group
  i_og_df <- i_group_df[, 1, drop = FALSE]
  i_og_df$a <- rowSums(i_group_df[-1])

  i_group_df_long <- data.frame(
         dates = c(0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 0L, 1L, 2L, 3L, 4L, 5L, 6L,
                   7L, 0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L),
        counts = c(1L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 0L, 1L, 0L, 0L, 0L, 0L, 0L,
                   0L, 0L, 0L, 2L, 1L, 0L, 1L, 0L, 0L),
        groups = as.factor(c("1", "1", "1", "1", "1", "1", "1", "1", "2", "2",
                             "2", "2", "2", "2", "2", "2", "3", "3", "3", "3",
                             "3", "3", "3", "3")), stringsAsFactors = TRUE
  )

  i <- incidence(dat, groups = fac)
  iog <- incidence(dat, groups = one_group)
  i.7 <- incidence(dat2, 7L, standard = TRUE)
  i.7.group <- incidence(dat2, 7L, standard = TRUE, groups = fac)
  df  <- as.data.frame(i)
  df1 <- as.data.frame(i, long = TRUE)
  df2 <- as.data.frame(incidence(1:2))
  df2res <- data.frame(
                       dates = c(1L, 2L),
                       counts = c(1L, 1L)
                       )
  df3 <- as.data.frame(i.7)
  df3res <- data.frame(
                       dates = as.Date(c("2015-12-28", "2016-01-04")),
                       weeks = c("2015-W53", "2016-W01"),
                       isoweeks = as.factor(c("2015-W53", "2016-W01")),
                       counts = c(2L, 5L)
                       )
  df3res$weeks <- aweek::date2week(df3res$dates, 1L, floor_day = TRUE, factor = TRUE)

  df4 <- as.data.frame(i.7, long = TRUE)
  df5 <- as.data.frame(i.7.group)
  df5res <- data.frame(
                       dates = as.Date(c("2015-12-28", "2016-01-04")),
                       weeks = c("2015-W53", "2016-W01"),
                       isoweeks = as.factor(c("2015-W53", "2016-W01")),
                       "1" = c(1L, 1L),
                       "2" = c(1L, 0L),
                       "3" = c(0L, 4L),
                       check.names = FALSE
                       )
  df5res$weeks <- aweek::date2week(df5res$dates, 1L, floor_day = TRUE, factor = TRUE)

  df6 <- as.data.frame(i.7.group, long = TRUE)
  df6res <- data.frame(
                       dates = as.Date(c("2015-12-28", "2016-01-04", "2015-12-28", "2016-01-04",
                                 "2015-12-28", "2016-01-04")),
                       weeks = c("2015-W53", "2016-W01", "2015-W53", "2016-W01", "2015-W53",
                                 "2016-W01"),
                       isoweeks = as.factor(c("2015-W53", "2016-W01", "2015-W53", "2016-W01",
                                              "2015-W53", "2016-W01")),
                       counts = c(1L, 1L, 1L, 0L, 0L, 4L),
                       groups = as.factor(c("1", "1", "2", "2", "3", "3"))
                       )
  df6res$weeks <- aweek::date2week(df6res$dates, 1L, floor_day = TRUE, factor = TRUE)
  
  df7 <- as.data.frame(iog)
  df7res <- data.frame(
                       dates = c(0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L),
                       a = c(1L, 1L, 2L, 1L, 0L, 1L, 0L, 1L)
                       )

  df8 <- as.data.frame(iog, long = TRUE)
  df8res <- data.frame(
                       dates = c(0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L),
                       counts = c(1L, 1L, 2L, 1L, 0L, 1L, 0L, 1L),
                       groups = as.factor(c("a", "a", "a", "a", "a", "a", "a", "a"))
                       )

  # expect_equal_to_reference(df, file = "rds/df.rds")
  expect_identical(df, i_group_df)
  # expect_equal_to_reference(dfl, file = "rds/dfl.rds")
  expect_identical(df1, i_group_df_long)
  # expect_equal_to_reference(df2, file = "rds/df2.rds")
  expect_identical(df2, df2res)
  # expect_equal_to_reference(df3, file = "rds/df3.rds")
  expect_identical(df3, df3res)
  expect_equal(df3, df4)
  # expect_equal_to_reference(df5, file = "rds/df5.rds")
  expect_identical(df5, df5res)
  # expect_equal_to_reference(df6, file = "rds/df6.rds")
  expect_identical(df6, df6res)
  expect_named(df7, c("dates", "a"))
  expect_named(df8, c("dates", "counts", "groups"))
})

test_that("as.incidence works", {
  skip_on_cran()

  dates_int <- sample(1:15, 100, replace = TRUE)
  dates <- as.Date("2017-04-01") + dates_int
  groups <- sample(letters[1:3], 100, replace = TRUE)

  i1 <- incidence(dates, interval = 2)
  i2 <- incidence(dates_int)
  i3 <- incidence(dates, interval = 7, groups = groups)
  i4 <- incidence(dates_int, interval = 7, groups = groups)

  expect_equal(as.incidence(i1$counts, i1$dates), i1)
  expect_equal(as.incidence(as.vector(i1$counts), i1$dates), i1)
  expect_equal(as.incidence(i2$counts, i2$dates), i2)
  expect_equal(as.incidence(i3$counts, i3$dates), i3)
  expect_equal(as.incidence(rep(1,10)), incidence(1:10))
  expect_equal(as.incidence(get_counts(i4), interval = 7L), i4)
  expect_equal(as.incidence(as.data.frame(get_counts(i4)), interval = 7L), i4)


  msg <- "Interval needs to be specified if there is only one date."
  expect_error(as.incidence(i3$counts[1,,drop = FALSE], i3$dates[1]),
               msg)

  msg <- "Columns should be named to label groups."
  expect_error(as.incidence(unname(i3$counts), i3$dates),
               msg)
})
