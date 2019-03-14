context("Test plotting")

test_that("plot for incidence object", {
  skip_on_cran()

  set.seed(1)
  # dat <- sample(1:50, 200, replace = TRUE, prob = 1 + exp(1:50 * 0.1))
  dat <- readRDS("data_cache/mfdat.rds")[1:200]
  dat2 <- as.Date("2016-01-02") + dat
  dat3 <- as.POSIXct(dat2)
  sex <- c(1, 1, 2, 1, 1, 2, 2, 1, 1, 2, 2, 1, 2, 2, 2, 1, 2, 2, 2, 1, 1, 1, 1,
           2, 2, 2, 1, 1, 1, 2, 1, 1, 1, 2, 1, 2, 2, 2, 1, 1, 1, 2, 2, 1, 1, 1,
           1, 1, 1, 2, 2, 2, 1, 2, 1, 1, 1, 1, 1, 2, 2, 1, 1, 2, 2, 1, 1, 2, 2,
           1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1,
           2, 1, 2, 2, 1, 1, 1, 2, 2, 1, 1, 1, 1, 2, 1, 2, 1, 1, 1, 1, 2, 2, 1,
           1, 1, 1, 2, 2, 2, 1, 1, 2, 2, 2, 2, 2, 1, 2, 2, 1, 1, 2, 1, 1, 1, 1,
           2, 2, 2, 1, 2, 1, 2, 1, 1, 1, 1, 2, 1, 1, 2, 1, 2, 1, 2, 1, 2, 1, 1,
           1, 2, 1, 2, 2, 1, 2, 2, 1, 1, 2, 2, 1, 1, 1, 1, 2, 1, 1, 1, 2, 2, 2,
           1, 1, 2, 2, 1, 2, 1, 1, 2, 2, 1, 1, 2, 1, 1, 1)
  sex <- ifelse(sex == 1, "female", "male")
  dat4 <- c(dat2,
            sample(dat2, replace = TRUE) + 50,
            sample(dat2, replace = TRUE) + 100
           )

  # constructing data
  i <- incidence(dat)
  iog <- incidence(dat, groups = rep("this group", 200))
  i.3 <- incidence(dat, 3L)
  i.14 <- incidence(dat, 14L)
  i.sex <- incidence(dat, 7L, groups = sex)

  # Dates ---------------------------------------------------------------------
  i.POSIX <- incidence(as.POSIXct(dat2, tz = "GMT"))
  i.isoweek <- incidence(dat2, 7L, standard = TRUE)

  # character intervals -------------------------------------------------------
  # Weekly intervals ------------------------------------------
  i.epiweek <- incidence(dat2, "1 epiweek", standard = TRUE)
  i.twoepiweek <- incidence(dat2, "2 epiweeks", standard = TRUE)
  i.sunweek <- incidence(dat2, "1 sunday week")
  i.monweek <- incidence(dat2, "1 monday week")
  i.tueweek <- incidence(dat2, "1 tuesday week")
  i.wedweek <- incidence(dat2, "1 wednesday week")
  i.thuweek <- incidence(dat2, "1 thursday week")
  i.friweek <- incidence(dat2, "1 friday week")
  i.satweek <- incidence(dat2, "1 saturday week")

  expect_identical(i.epiweek, i.sunweek)
  expect_identical(i.isoweek$weeks, i.monweek$weeks)
  expect_identical(get_counts(i.isoweek), get_counts(i.monweek))

  # months and quarters ---------------------------------------
  i.sexmonth <- incidence(dat4, "1 month", groups = rep(sex, 3))
  i.sexquarter <- incidence(dat4, "1 quarter", groups = rep(sex, 3))
  # special case for fit_optim_split:
  i.sex.o <- incidence(c(dat, abs(dat - 45) + 45), 7L, groups = c(sex, rev(sex)))

  fit.i <- suppressWarnings(fit(i))
  fit.i.2 <- suppressWarnings(fit(i, split = 30))
  fit.i.3 <- suppressWarnings(fit(i.3[5:13]))
  fit.POSIX <- suppressWarnings(fit(i.POSIX))
  fit.sex <- suppressWarnings(fit(i.sex))
  fit.sex.o <- suppressWarnings(fit_optim_split(i.sex.o))
  fit.o <- suppressWarnings(fit_optim_split(pool(i.sex.o)))
  p.fit.i <- plot(fit.i)
  p.fit.i.2 <- plot(i, fit = fit.i.2, color = "lightblue")
  p.fit.sex <- plot(fit.sex)
  p.optim.sex   <- fit.sex.o$plot
  p.optim.sex.fit <- plot(fit.sex.o$fit)
  p.optim   <- fit.o$plot
  p.i <- plot(i)
  p.i.cum <- plot(cumulate(i))
  p.i.square <- plot(i, show_cases = TRUE)
  

  p.i.14 <- plot(i.14)
  p.i.2 <- plot(i, color = "blue", alpha = .2)
  p.i.3 <- plot(i.3, fit = fit.i.3, color = "red")
  p.sex <- plot(i.sex)
  p.sex.cum <- plot(cumulate(i.sex))
  p.sex.2 <- plot(i.sex, fit = fit.sex)
  suppressMessages(p.sex.o <- plot(i.sex, fit = fit.sex.o$fit))
  p.sex.3 <- plot(i.sex, fit = fit.sex, col_pal = rainbow)
  p.sex.4 <- plot(i.sex, fit = fit.sex,
                  color = c(male = "salmon3", female = "gold2"))
  p.isoweek   <- plot(i.isoweek)
  p.isoweek.2 <- plot(i.isoweek, labels_week = FALSE)
  p.epiweek   <- plot(i.epiweek)
  p.epiweek.2 <- plot(i.epiweek, labels_week = FALSE)
  p.epiweek.b <- plot(i.epiweek, labels_week = FALSE, n_breaks = nrow(i.epiweek))
  p.twoepiweek <- plot(i.twoepiweek, n_breaks = nrow(i.twoepiweek))
  
  p.sunweek   <- plot(i.sunweek)
  expect_warning({
    p.sunweek.2 <- plot(i.sunweek, labels_iso = FALSE)
  }, "labels_iso is deprecated. Use `labels_week` instead")
  p.monweek   <- plot(i.monweek)
  expect_warning({
    p.monweek.2 <- plot(i.monweek, labels_week = FALSE, labels_iso = TRUE)
  }, "labels_iso is deprecated. The value of `labels_week` will be used")
  p.tueweek   <- plot(i.tueweek)
  p.tueweek.2 <- plot(i.tueweek, labels_week = FALSE)
  p.wedweek   <- plot(i.wedweek)
  p.wedweek.2 <- plot(i.wedweek, labels_week = FALSE)
  p.thuweek   <- plot(i.thuweek)
  p.thuweek.2 <- plot(i.thuweek, labels_week = FALSE)
  p.friweek   <- plot(i.friweek)
  p.friweek.2 <- plot(i.friweek, labels_week = FALSE)
  p.satweek   <- plot(i.satweek)
  p.satweek.2 <- plot(i.satweek, labels_week = FALSE)
  
  p.POSIX     <- plot(i.POSIX)
  p.POSIX.f   <- plot(i.POSIX, fit = fit.POSIX)
  p.month     <- plot(i.sexmonth)
  p.quarter   <- plot(i.sexquarter)

  ## messages
  expect_message(plot(i.sex, show_cases = TRUE, stack = FALSE),
                 "`show_cases` requires the argument `stack = TRUE`")
  expect_message(p.iog <- plot(iog, color = c("this group" = "blue", "that group" = "red")), 
                 "1 colors were not used: \"that group\" = \"red\"")

  ## errors
  expect_error(plot(i, fit = "tamere"),
               "Fit must be a 'incidence_fit' object, or a list of these")
  expect_error(plot(i, fit = list(fit.i, "tamere")),
               "The 2-th item in 'fit' is not an 'incidence_fit' object, but a character")

  ## Normal plots
  vdiffr::expect_doppelganger("incidence fit", p.fit.i)
  vdiffr::expect_doppelganger("incidence plot with two fitting models", p.fit.i.2)
  vdiffr::expect_doppelganger("grouped incidence fit", p.fit.sex)
  vdiffr::expect_doppelganger("incidence plot with default interval", p.i)
  vdiffr::expect_doppelganger("incidence plot with default interval, cumulative", p.i.cum)
  vdiffr::expect_doppelganger("incidence plot with interval of 14 days", p.i.14)
  vdiffr::expect_doppelganger("incidence plot with specified color and alpha", p.i.2)
  vdiffr::expect_doppelganger("incidence plot with interval of 3 days, fit and specified color", p.i.3)
  vdiffr::expect_doppelganger("grouped incidence plot", p.sex)
  vdiffr::expect_doppelganger("grouped incidence plot with one group", p.iog)
  vdiffr::expect_doppelganger("grouped incidence plot, cumulative", p.sex.cum)
  vdiffr::expect_doppelganger("grouped incidence plot with fit", p.sex.2)
  vdiffr::expect_doppelganger("grouped incidence plot with color palette", p.sex.3)
  vdiffr::expect_doppelganger("grouped incidence plot with specified color", p.sex.4)
  vdiffr::expect_doppelganger("incidence plot from POSIXct data", p.POSIX)
  vdiffr::expect_doppelganger("incidence plot from POSIXct data with fit", p.POSIX.f)
  vdiffr::expect_doppelganger("incidence plot with isoweek labels", p.isoweek)
  vdiffr::expect_doppelganger("incidence plot without isoweek labels", p.isoweek.2)
  vdiffr::expect_doppelganger("incidence plot by month", p.month)
  vdiffr::expect_doppelganger("incidence plot by quarter", p.quarter)
  vdiffr::expect_doppelganger("incidence fit plot with split", p.sex.o)
  vdiffr::expect_doppelganger("incidence fit list plot with split", p.optim.sex.fit)
  vdiffr::expect_doppelganger("split optimum plot", p.optim.sex)
  vdiffr::expect_doppelganger("split optimum plot pooled", p.optim)
  vdiffr::expect_doppelganger("epiquares single plot", p.i.square)

  ## Weekly plots
  vdiffr::expect_doppelganger("sun weekly incidence with labels", p.sunweek)
  vdiffr::expect_doppelganger("sun weekly incidence with labels", p.epiweek)
  vdiffr::expect_doppelganger("sun weekly incidence with dates", p.sunweek.2)
  vdiffr::expect_doppelganger("sun weekly incidence with dates", p.epiweek.2)
  vdiffr::expect_doppelganger("sun weekly incidence with dates and full breaks", p.epiweek.b)
  vdiffr::expect_doppelganger("sun semi-weekly incidence with dates and full breaks", p.twoepiweek)


  vdiffr::expect_doppelganger("mon weekly incidence with labels", p.monweek)
  vdiffr::expect_doppelganger("mon weekly incidence with dates", p.monweek.2)

  vdiffr::expect_doppelganger("tue weekly incidence with labels", p.tueweek)
  vdiffr::expect_doppelganger("tue weekly incidence with dates", p.tueweek.2)
  
  vdiffr::expect_doppelganger("wed weekly incidence with labels", p.wedweek)
  vdiffr::expect_doppelganger("wed weekly incidence with dates", p.wedweek.2)
  
  vdiffr::expect_doppelganger("thu weekly incidence with labels", p.thuweek)
  vdiffr::expect_doppelganger("thu weekly incidence with dates", p.thuweek.2)
  
  vdiffr::expect_doppelganger("fri weekly incidence with labels", p.friweek)
  vdiffr::expect_doppelganger("fri weekly incidence with dates", p.friweek.2)
  
  vdiffr::expect_doppelganger("sat weekly incidence with labels", p.satweek)
  vdiffr::expect_doppelganger("sat weekly incidence with dates", p.satweek.2)
})
