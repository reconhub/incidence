context("Test plotting")


test_that("plot for incidence object", {
    set.seed(1)
    dat <- sample(1:50, 200, replace = TRUE, prob = 1 + exp(1:50 * 0.1))
    sex <- sample(c("female", "male"), 200, replace = TRUE)

    i <- incidence(dat, 3L)
    i.sex <- incidence(dat, 7L, groups = sex)
    fit.i <- suppressWarnings(fit(i))
    fit.sex <- suppressWarnings(fit(i.sex))

    p.fit.i <- plot(fit.i)
    p.fit.sex <- plot(fit.sex)
    p.i <- plot(i)
    p.i.2 <- plot(i, color = "blue", alpha=.2)
    p.i.3 <- plot(i, fit = fit.i, color = "red")
    p.sex <- plot(i.sex)
    p.sex.2 <- plot(i.sex, fit=fit.sex)
    p.sex.3 <- plot(i.sex, fit=fit.sex, col_pal = rainbow)
    p.sex.4 <- plot(i.sex, fit=fit.sex,
                   color = c(male = "salmon3",female = "gold2"))

    expect_equal_to_reference(p.fit.i, file = "rds/p.fit.i.rds")
    expect_equal_to_reference(p.fit.sex, file = "rds/p.fit.sex.rds")
    expect_equal_to_reference(p.i, file = "rds/p.i.rds")
    expect_equal_to_reference(p.i.2, file = "rds/p.i.2.rds")
    expect_equal_to_reference(p.i.3, file = "rds/p.i.3.rds")
    expect_equal_to_reference(p.sex, file = "rds/p.sex.rds")
    expect_equal_to_reference(p.sex.2, file = "rds/p.sex.2.rds")
    expect_equal_to_reference(p.sex.3, file = "rds/p.sex.3.rds")
    expect_equal_to_reference(p.sex.4, file = "rds/p.sex.4.rds")

})
