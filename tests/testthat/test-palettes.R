context("Color palettes")

test_that("pal1", {
    expect_error(pal1(NULL), "n is not a number")
    invisible(sapply(1:30, function(n) expect_length(pal1(n), n)))
})



test_that("pal1light", {
    expect_error(pal1light(NULL), "n is not a number")
    invisible(sapply(1:30, function(n) expect_length(pal1light(n), n)))
})



test_that("pal1dark", {
    expect_error(pal1dark(NULL), "n is not a number")
    invisible(sapply(1:30, function(n) expect_length(pal1dark(n), n)))
})
