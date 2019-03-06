context("standardisation tests") 


d <- c('2019-04-18', '2019-04-14', '2019-03-31', '2019-04-03', '2019-03-30',
       '2019-04-05', '2019-03-12', '2019-04-07', '2019-04-02', '2019-03-09',
       '2019-04-20', '2019-04-23', '2019-03-07', '2019-03-25', '2019-03-27',
       '2019-04-13', '2019-04-15', '2019-04-04', '2019-03-30', '2019-03-19')

test_that("standard will override first_date", {
       
  expect_output(print(incidence(d, interval = "week", standard = TRUE)), "2019-03-04")       
  expect_output(print(incidence(d, interval = "week", standard = TRUE)), "2019-W10")       
  expect_output(print(incidence(d, interval = "week", standard = FALSE)), "2019-03-07")       

  expect_output(print(incidence(d, interval = "month", standard = TRUE)), "2019-03-01")       
  expect_output(print(incidence(d, interval = "month", standard = FALSE)), "2019-03-07")       
       
  expect_output(print(incidence(d, interval = "year", standard = TRUE)), "2019-01-01")       
  expect_output(print(incidence(d, interval = "year", standard = FALSE)), "2019-03-07")       
       
})
