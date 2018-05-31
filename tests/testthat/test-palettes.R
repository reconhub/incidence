context("Color palettes")

test_that("incidence_pal1", {
  skip_on_cran()

  expect_error(incidence_pal1(NULL), "n is not a number")
  for (n in 1:30) {
    expect_length(incidence_pal1(n), n)
  }
})

test_that("incidence_pal1_light", {
  skip_on_cran()

  expect_error(incidence_pal1_light(NULL), "n is not a number")
  for (n in 1:30) {
    expect_length(incidence_pal1_light(n), n)
  }
})



test_that("incidence_pal1_dark", {
  skip_on_cran()

  expect_error(incidence_pal1_dark(NULL), "n is not a number")
  for (n in 1:30) {
    expect_length(incidence_pal1_dark(n), n)
  }
})
