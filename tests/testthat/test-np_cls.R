
test_that("Parameter ranges works ok", {
  expect_error(np_cls(C = 1.95, e = 0.04, sd_exp = 0.726, m = 8, N = 500), "Parameter 'C' must be in the range 0 <= C <= 1")
  expect_error(np_cls(C = -0.95, e = 0.04, sd_exp = 0.726, m = 8, N = 500), "Parameter 'C' must be in the range 0 <= C <= 1")
  expect_error(np_cls(C = 0.95, e = 1.04, sd_exp = 0.726, m = 8, N = 500), "Parameter 'e' must be in the range 0 <= e <= 1")
  expect_error(np_cls(C = 0.95, e = -0.04, sd_exp = 0.726, m = 8, N = 500), "Parameter 'e' must be in the range 0 <= e <= 1")
  expect_error(np_cls(C = 0.95, e = 0.04, sd_exp = -0.726, m = 8, N = 500), "Parameter 'sd_exp' must be a positive number")
  expect_error(np_cls(C = 0.95, e = 0.04, sd_exp = 0.726, m = -8, N = 500), "Parameter 'm' must be a positive number")
  expect_error(np_cls(C = 0.95, e = 0.04, sd_exp = 0.726, m = 8, N = 500.1), "Parameter 'N' must be a positive integer or Inf")
  expect_error(np_cls(C = 0.95, e = 0.04, sd_exp = 0.726, m = 8, N = -500), "Parameter 'N' must be a positive integer or Inf")
})

test_that("np_cls works", {
  result <- np_cls(C = 0.95, e = 0.04, sd_exp = 0.726, m = 8, N = 500)
  expect_equal(result$n, 20)
})

test_that("np_cls works for 8.10", {
  result <- np_cls(C = 0.95, e = 0.04, sd_exp = 0.726, m = 6.04, N = 415, parameter = TRUE)
  expect_equal(result$n, 33)
})


