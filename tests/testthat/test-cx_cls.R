
test_that("Parameter ranges works ok", {
  expect_error(cx_cls(C = 1.95, x_est = 9990, n_real = 30, N = 500, m = 8, sd_est = 15000, parameter = TRUE), "Parameter 'C' must be in the range 0 <= C <= 1")
  expect_error(cx_cls(C = -0.95, x_est = 9990, n_real = 30, N = 500, m = 8, sd_est = 15000, parameter = TRUE), "Parameter 'C' must be in the range 0 <= C <= 1")
  expect_error(cx_cls(C = 0.95, x_est = 9990, n_real = 30.1, N = 500, m = 8, sd_est = 15000, parameter = TRUE), "Parameter 'n_real' must be a positive integer")
  expect_error(cx_cls(C = 0.95, x_est = 9990, n_real = -30, N = 500, m = 8, sd_est = 15000, parameter = TRUE), "Parameter 'n_real' must be a positive integer")
  expect_error(cx_cls(C = 0.95, x_est = 9990, n_real = 30, N = 500, m = 8, sd_est = -15000, parameter = TRUE), "Parameter 'sd_est' must be a positive number")
  expect_error(cx_cls(C = 0.95, x_est = 9990, n_real = 30, N = 500, m = -8, sd_est = 15000, parameter = TRUE), "Parameter 'm' must be a positive number")
  expect_error(cx_cls(C = 0.95, x_est = 9990, n_real = 30, N = 500.1, m = 8, sd_est = 15000, parameter = TRUE), "Parameter 'N' must be a positive integer or Inf")
  expect_error(cx_cls(C = 0.95, x_est = 9990, n_real = 30, N = -500, m = 8, sd_est = 15000, parameter = TRUE), "Parameter 'N' must be a positive integer or Inf")
})

# Ref. to 8.2
test_that("cx_cls works", {
  result <- cx_cls(C = 0.95, x_est = 9990, n_real = 30, N = 500, m = 8, sd_est = 15000, parameter = TRUE)
  expect_equal(round(result$margin_of_error,3), 650.508)
  result <- cx_cls(C = 0.95, x_est = 9990, n_real = 19, N = 500, m = 8, sd_est = 15000, parameter = TRUE)
  expect_equal(round(result$margin_of_error,3), 826.914)
  expect_equal(result$inference, "The population mean is between 9163.086 and 10816.914 with 95% confidence. Note that n_real < 20 and the estimated variance may not be unbiased. Consider increasing the sample size.")
})

test_that("cx_cls works for example 8.2", {
  result <- cx_cls(C = 0.95, x_est = 8801, n_real = 25, N = 415, m = 6.04, sd_est = 25189, parameter = TRUE)
  expect_equal(round(result$margin_of_error,3), 1584.748)
})
