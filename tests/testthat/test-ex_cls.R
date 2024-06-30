
test_that("Parameter ranges works ok", {
  expect_error(ex_cls(C = 1.95, n_real = 31, sd_est = 15000, m = 8, N = 500, parameter = TRUE), "Parameter 'C' must be in the range 0 <= C <= 1")
  expect_error(ex_cls(C = -0.95, n_real = 31, sd_est = 15000, m = 8, N = 500, parameter = TRUE), "Parameter 'C' must be in the range 0 <= C <= 1")
  expect_error(ex_cls(C = 0.95, n_real = 31.1, sd_est = 15000, m = 8, N = 500, parameter = TRUE), "Parameter 'n_real' must be a positive integer")
  expect_error(ex_cls(C = 0.95, n_real = -31, sd_est = 15000, m = 8, N = 500, parameter = TRUE), "Parameter 'n_real' must be a positive integer")
  expect_error(ex_cls(C = 0.95, n_real = 31, sd_est = -15000, m = 8, N = 500, parameter = TRUE), "Parameter 'sd_est' must be a positive number")
  expect_error(ex_cls(C = 0.95, n_real = 31, sd_est = 15000, m = -8, N = 500, parameter = TRUE), "Parameter 'm' must be a positive number")
  expect_error(ex_cls(C = 0.95, n_real = 31, sd_est = 15000, m = 8, N = 500.1, parameter = TRUE), "Parameter 'N' must be a positive integer or Inf")
  expect_error(ex_cls(C = 0.95, n_real = 31, sd_est = 15000, m = 8, N = -500, parameter = TRUE), "Parameter 'N' must be a positive integer or Inf")
})

test_that("ex_cls works", {
  result <- ex_cls(C = 0.95, n_real = 31, sd_est = 15000, m = 8, N = 500, parameter = TRUE)
  expect_equal(round(result$E, 3), 639.249)
})

