context("incidence object accessor tests")

set.seed(999)
int   <- sample(-3:50, 100, replace = TRUE)
dat   <- as.Date("2018-01-31") + int
grp   <- sample(letters[1:3], length(dat), replace = TRUE)
xg    <- incidence(dat, groups = grp)
x.1   <- incidence(int)
x.7   <- incidence(int, 7L)
x.day <- incidence(dat, "day")
x.wee <- incidence(dat, "week")
x.mon <- incidence(dat, "month")
x.mon2yr <- incidence(c(as.Date("2017-12-15"), dat), "month")
x.yer <- incidence(c(dat, dat - 365, dat - 365 * 2), "year")

test_that("get_interval works for integers", {
  expect_equal(get_interval(x.1), 1L)
  expect_equal(get_interval(x.1, integer = FALSE), 1L)
})

test_that("get_timespan works", {
  expect_equal(get_timespan(x.1), x.1$timespan)
  expect_equal(get_timespan(x.mon), x.mon$timespan)
  expect_error(get_timespan("coffee"), "Not implemented for class character")
})

test_that("get_n works", {
  expect_equal(get_n(x.1), x.1$n)
  expect_equal(get_n(x.7), x.7$n)
  expect_equal(get_n(x.mon), x.mon$n)
  expect_error(get_n("coffee"), "Not implemented for class character")
})

test_that("group_names works", {
  expect_identical(group_names(xg), letters[1:3])
  expect_null(group_names(x.1))
  group_names(xg) <- c("foo", "bar", "baz")
  expect_identical(group_names(xg), c("foo", "bar", "baz"))
  expect_error(group_names(letters), "Not implemented for class character")
  expect_error(group_names(letters) <- 1:10)
})

test_that("group_names can collapse groups", {
  xg2 <- group_names(xg, rep("a", 3))
  xg3 <- group_names(xg, c("a", "b", "b"))
  expect_error(group_names(xg, letters[1:4]), "value must be the same length as the number of groups")
  expect_error(group_names(xg, c(letters[1:2], NA)), "value must be able to be coerced to a character vector")
  expect_equal(ncol(xg3), 2L)
  expect_equal(sum(get_counts(xg3)), sum(get_counts(xg)))
  expect_equal(colSums(get_counts(xg3))[["b"]], sum(colSums(get_counts(xg))[c("b", "c")]))
  expect_equivalent(xg2, pool(xg))
})

test_that("ncol works", {
  expect_equal(ncol(xg), 3L)
  expect_equal(ncol(x.1), 1L)
  expect_equal(ncol(subset(xg, groups = 1:2)), 2L)
})

test_that("get_dates works for integers", {
  expect_equal(get_dates(x.1), x.1$dates)
  expect_equal(get_dates(x.1, count_days = TRUE), seq_along(x.1$dates) - 1.0)
  expect_equal(get_dates(x.1, "center"), x.1$dates + 0.5)
  expect_equal(get_dates(x.1, "right"),  x.1$dates + 1)
})

test_that("get_dates borks correctly", {
  expect_error(get_dates("grind", "Not implemented for class character"))
})

test_that("get_interval works for integer weeks", {
  expect_equal(get_interval(x.7), 7L)
  expect_equal(get_interval(x.7, integer = FALSE), 7L)
  expect_error(get_interval("pizza"), "Not implemented for class character")
})

test_that("get_dates works for integer weeks", {
  expect_equal(get_dates(x.7), x.7$dates)
  expect_equal(get_dates(x.7, count_days = TRUE), 7 * (seq_along(x.7$dates) - 1.0))
  expect_equal(get_dates(x.7, "center"), x.7$dates + 3.5)
  expect_equal(get_dates(x.7, "right"),  x.7$dates + 7)
})


test_that("get_interval works for character days", {
  expect_equal(get_interval(x.day), 1L)
  expect_equal(get_interval(x.day, integer = FALSE), "day")
})

test_that("get_dates works for character days", {
  expect_equal(get_dates(x.day), x.day$dates)
  expect_equal(get_dates(x.day, count_days = TRUE), (seq_along(x.day$dates) - 1.0))
  expect_equal(get_dates(x.day, "center"), x.day$dates + 0.5)
  expect_equal(get_dates(x.day, "right"),  x.day$dates + 1.0)
})

test_that("get_interval works for character weeks", {
  expect_equal(get_interval(x.wee), 7L)
  expect_equal(get_interval(x.wee, integer = FALSE), "week")
})

test_that("get_dates works for character weeks", {
  expect_equal(get_dates(x.wee), x.wee$dates)
  expect_equal(get_dates(x.wee, count_days = TRUE), 7 * (seq_along(x.wee$dates) - 1.0))
  expect_equal(get_dates(x.wee, "center"), x.wee$dates + 3.5)
  expect_equal(get_dates(x.wee, "right"),  x.wee$dates + 7.0)
})


test_that("get_interval works for character months", {
  expect_equal(get_interval(x.mon), c(31, 28, 31))
  expect_equal(get_interval(x.mon, integer = FALSE), "month")
  expect_equal(get_interval(x.mon2yr), c(31, 31, 28, 31))
})

test_that("get_interval works for character years", {
  expect_equal(get_interval(x.yer), c(366, 365, 365))
  expect_equal(get_interval(x.yer, integer = FALSE), "year")
})

test_that("get_dates works for character months", {
  expect_equal(get_dates(x.mon), x.mon$dates)
  expect_equal(get_dates(x.mon, count_days = TRUE), c(0, 31, 59))
  expect_equal(get_dates(x.mon, "center"), x.mon$dates + c(31, 28, 31) / 2)
  expect_equal(get_dates(x.mon, "right"),  x.mon$dates + c(31, 28, 31))
})

test_that("errors happen", {
  xx          <- x.1
  xx$interval <- factor("what")
  expect_error(get_interval(xx), "factor")
})




# Data for the get_info and get_fit accessors
set.seed(1)
dat2 <- sample(1:50, 200, replace = TRUE, prob = 1 + exp(1:50 * 0.1))
sex <- sample(c("female", "male"), 200, replace = TRUE)
i.sex.o <- incidence(c(dat2, abs(dat2 - 45) + 45), 7L, groups = c(sex, rev(sex)))

test_that("get_counts works with and without groups", {
  expect_is(get_counts(i.sex.o), "matrix")
  expect_identical(get_counts(i.sex.o), i.sex.o$counts)
  expect_identical(get_counts(i.sex.o, "female"), get_counts(i.sex.o, 1))
  expect_message(get_counts(i.sex.o, c("female", "nb")),
		 "The following groups were not recognised: nb")
  expect_error(suppressMessages(get_counts(i.sex.o, "what")),
	       "No groups matched those present in the data: female, male")
})

context("incidence_fit* object accessor tests")


i.fitlist <- fit_optim_split(i.sex.o)
fits <- get_fit(i.fitlist$fit)

# Creating an `incidence_fit_list` object with no groups column
fits_list <- fits
for (i in names(fits)) {
  fits_list[[i]]$info$pred$groups <- NULL  
}
class(fits_list) <- "incidence_fit_list"
attr(fits_list, "locations") <- as.list(names(fits))

test_that("fit_optim_split() returns an incidence_fit_list", {
  expect_is(i.fitlist$fit, "incidence_fit_list")
})

test_that("get_fit() returns a list of incidence fit objects", {
  for (i in names(fits)) {
    expect_is(fits[[i]], "incidence_fit", label = i)
  }
  expect_identical(fits[[1]], get_fit(fits[[1]]))
})

test_that("get_info() will return a vector for r", {
  rvec <- get_info(i.fitlist$fit, "r")
  expect_length(rvec, 4) 
}) 

test_that("get_info() will return a vector for doubling/halving", {
  dvec    <- get_info(i.fitlist$fit, "doubling")
  dvec.na <- get_info(i.fitlist$fit, "doubling", na.rm = FALSE)
  expect_length(dvec, 2)
  expect_length(dvec.na, 4)
  expect_identical(dvec, dvec.na[1:2])

  hvec    <- get_info(i.fitlist$fit, "halving")
  hvec.na <- get_info(i.fitlist$fit, "halving", na.rm = FALSE)
  expect_length(hvec, 2)
  expect_length(hvec.na, 4)
  expect_identical(hvec, hvec.na[3:4])
})

test_that("get_info() will return a data frame for pred", {
  # Should have groups be female and male
  pred.g  <- get_info(i.fitlist$fit, "pred")
  # Should have no groups
  pred.ng <- get_info(fits_list, "pred", groups = NULL)
  # Should have groups be female and male
  pred.g1 <- get_info(fits_list, "pred", groups = 1)
  # Should have groups be before and after
  pred.g2 <- get_info(i.fitlist$fit, "pred", groups = 2)
  
  expect_null(pred.ng$groups)
  expect_identical(pred.g$groups, pred.g1$groups)
  expect_identical(levels(pred.g2$groups), c("before", "after"))
})

test_that("get_info() will return matrices for *.conf", {
  hconf    <- get_info(i.fitlist$fit, "halving.conf")
  hconf.na <- get_info(i.fitlist$fit, "halving.conf", na.rm = FALSE)
  expect_is(hconf, "matrix")
  expect_is(hconf.na, "matrix")
  expect_length(hconf, 4)
  expect_identical(hconf, hconf.na[-(1:2), ])
})
