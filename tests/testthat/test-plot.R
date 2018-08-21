context("Test plotting")

test_that("plot for incidence object", {
  skip_on_cran()

  set.seed(1)
  dat <- sample(1:50, 200, replace = TRUE, prob = 1 + exp(1:50 * 0.1))
  dat2 <- as.Date("2016-01-02") + dat
  dat3 <- as.POSIXct(dat2)
  sex <- sample(c("female", "male"), 200, replace = TRUE)
  dat4 <- c(dat2,
            sample(dat2, replace = TRUE) + 50,
            sample(dat2, replace = TRUE) + 100
           )

  # constructing data
  i <- incidence(dat)
  i.3 <- incidence(dat, 3L)
  i.14 <- incidence(dat, 14L)
  i.sex <- incidence(dat, 7L, groups = sex)
  i.POSIX <- incidence(as.POSIXct(dat2, tz = "GMT"))
  i.isoweek <- incidence(dat2, 7L, standard = TRUE)
  i.sexmonth <- incidence(dat4, "month", groups = rep(sex, 3))
  i.sexquarter <- incidence(dat4, "quarter", groups = rep(sex, 3))
  # special case for fit_optim_split:
  i.sex.o <- incidence(c(dat, abs(dat - 45) + 45), 7L, groups = c(sex, rev(sex)))

  fit.i <- suppressWarnings(fit(i))
  fit.i.2 <- suppressWarnings(fit(i, split = 30))
  fit.i.3 <- suppressWarnings(fit(i.3[5:13]))
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
  p.isoweek <- plot(i.isoweek)
  p.POSIX <- plot(i.POSIX)
  p.isoweek.2 <- plot(i.isoweek, labels_iso = FALSE)
  p.month <- plot(i.sexmonth)
  p.quarter <- plot(i.sexquarter)

  vdiffr::expect_doppelganger("incidence fit", p.fit.i)
  vdiffr::expect_doppelganger("incidence plot with two fitting models", p.fit.i.2)
  vdiffr::expect_doppelganger("grouped incidence fit", p.fit.sex)
  vdiffr::expect_doppelganger("incidence plot with default interval", p.i)
  vdiffr::expect_doppelganger("incidence plot with default interval, cumulative", p.i.cum)
  vdiffr::expect_doppelganger("incidence plot with interval of 14 days", p.i.14)
  vdiffr::expect_doppelganger("incidence plot with specified color and alpha", p.i.2)
  vdiffr::expect_doppelganger("incidence plot with interval of 3 days, fit and specified color", p.i.3)
  vdiffr::expect_doppelganger("grouped incidence plot", p.sex)
  vdiffr::expect_doppelganger("grouped incidence plot, cumulative", p.sex.cum)
  vdiffr::expect_doppelganger("grouped incidence plot with fit", p.sex.2)
  vdiffr::expect_doppelganger("grouped incidence plot with color palette", p.sex.3)
  vdiffr::expect_doppelganger("grouped incidence plot with specified color", p.sex.4)
  vdiffr::expect_doppelganger("incidence plot from POSIXct data", p.POSIX)
  vdiffr::expect_doppelganger("incidence plot with isoweek labels", p.isoweek)
  vdiffr::expect_doppelganger("incidence plot without isoweek labels", p.isoweek.2)
  vdiffr::expect_doppelganger("incidence plot by month", p.month)
  vdiffr::expect_doppelganger("incidence plot by quarter", p.quarter)
  vdiffr::expect_doppelganger("incidence fit plot with split", p.sex.o)
  vdiffr::expect_doppelganger("incidence fit list plot with split", p.optim.sex.fit)
  vdiffr::expect_doppelganger("split optimum plot", p.optim.sex)
  vdiffr::expect_doppelganger("split optimum plot pooled", p.optim)

  ## errors
  expect_error(plot(i, fit = "tamere"),
               "Fit must be a 'incidence_fit' object, or a list of these")
  expect_error(plot(i, fit = list(fit.i, "tamere")),
               "The 2-th item in 'fit' is not an 'incidence_fit' object, but a character")
})
