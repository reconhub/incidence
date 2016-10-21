context("Subset of incidence objects")


test_that("[ operator for incidence objects", {
    set.seed(as.numeric(Sys.time()))
    dat <- as.integer(sample(-3:10, 50, replace=TRUE))
    x <- incidence(dat)

    x.sub1 <- x[c(3,5,7,8)]
    expect_equal_to_reference(x.sub1, file = "rds/x.sub1.rds")

    x.sub2 <- x[-c(5,1,2)]
    expect_equal_to_reference(x.sub2, file = "rds/x.sub2.rds")
})




test_that("subset for incidence objects", {
    set.seed(as.numeric(Sys.time()))
    dat <- as.integer(sample(-3:10, 50, replace=TRUE))
    x <- incidence(dat)

    x.sub3 <- subset(x, from = 0)
    expect_equal_to_reference(x.sub3, file = "rds/x.sub3.rds")

    x.sub4 <- subset(x, to = 5)
    expect_equal_to_reference(x.sub4, file = "rds/x.sub4.rds")

    x.sub5 <- subset(x, from = 1, to = 4)
    expect_equal_to_reference(x.sub5, file = "rds/x.sub5.rds")

})
