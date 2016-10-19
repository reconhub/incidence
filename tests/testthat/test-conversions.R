context("Conversions of incidence objects")


test_that("as.data.frame works", {
    dat <- as.integer(c(0,1,2,2,3,5,7))
    fac <- factor(c(1, 2, 3, 3, 3, 3, 1))
    i <- incidence(dat, groups=fac)
    df <- as.data.frame(i)
    dfl <- as.data.frame(i, long = TRUE)

    expect_identical(df,
                     structure(list(dates = 0:7,
                                    `1` = c(1L, 0L, 0L, 0L, 0L, 0L, 0L, 1L),
                                    `2` = c(0L, 1L, 0L, 0L, 0L, 0L, 0L, 0L),
                                    `3` = c(0L, 0L, 2L, 1L, 0L, 1L, 0L, 0L)),
                               .Names = c("dates", "1", "2", "3"),
                               row.names = c(NA, -8L), class = "data.frame")
                     )

    expect_identical(dfl,
                     structure(list(
                         dates = c(0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 0L,
                         1L, 2L, 3L, 4L, 5L, 6L, 7L, 0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L),
                         counts = c(1L, 0L, 0L, 0L, 0L, 0L, 0L, 1L, 0L, 1L, 0L, 0L,
                         0L, 0L, 0L, 0L, 0L, 0L, 2L, 1L, 0L, 1L, 0L, 0L),
                         groups = structure(c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L,
                         2L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L),
                         .Label = c("1", "2", "3"), class = "factor")),
                               .Names = c("dates", "counts", "groups"),
                               row.names = c(NA, -24L), class = "data.frame")
                     )
})
