
test_that("Parameter ranges works ok", {
  expect_error(ct_cls(C = 1.95, x_est = 9990, n_real = 30, N = 500, m = 8, M = 2500, sd_est = 15000, TRUE), "Parameter 'C' must be in the range 0 <= C <= 1")
  expect_error(ct_cls(C = -0.95, x_est = 9990, n_real = 30, N = 500, m = 8, M = 2500, sd_est = 15000, TRUE), "Parameter 'C' must be in the range 0 <= C <= 1")
  expect_error(ct_cls(C = 0.95, x_est = 9990, n_real = 30.1, N = 500, m = 8, M = 2500, sd_est = 15000, TRUE), "Parameter 'n_real' must be a positive integer")
  expect_error(ct_cls(C = 0.95, x_est = 9990, n_real = -30, N = 500, m = 8, M = 2500, sd_est = 15000, TRUE), "Parameter 'n_real' must be a positive integer")
  expect_error(ct_cls(C = 0.95, x_est = 9990, n_real = 30, N = 500, m = 8, M = 2500, sd_est = -15000, TRUE), "Parameter 'sd_est' must be a positive number")
  expect_error(ct_cls(C = 0.95, x_est = 9990, n_real = 30, N = 500, m = -8, M = 2500, sd_est = 15000, TRUE), "Parameter 'm' must be a positive number")
  expect_error(ct_cls(C = 0.95, x_est = 9990, n_real = 30, N = 500, m = 8, M = -2500, sd_est = 15000, TRUE), "Parameter 'M' must be Null or a positive number")
  expect_error(ct_cls(C = 0.95, x_est = 9990, n_real = 30, N = 500.1, m = 8, M = 2500, sd_est = 15000, TRUE), "Parameter 'N' must be a positive integer or Inf")
  expect_error(ct_cls(C = 0.95, x_est = 9990, n_real = 30, N = -500, m = 8, M = 2500, sd_est = 15000, TRUE), "Parameter 'N' must be a positive integer or Inf")
})

# Ref. to 8.4 / 8.7
test_that("ct_cls works", {
  result <- ct_cls(C = 0.95, x_est = 9990, n_real = 30, N = 500, m = 8, M = 2500, sd_est = 15000, TRUE)
  expect_equal(round(result$t_est,0), 24975000)
  expect_equal(round(result$margin_of_error,0), 1626270)
  result <- ct_cls(C = 0.95, x_est = 9990, n_real = 19, N = 500, m = 8, sd_est = 15000, parameter = TRUE)
  expect_equal(round(result$t_est,0), 39960000)
  expect_equal(round(result$margin_of_error,0), 3307654)
  expect_equal(result$inference, "The population mean is between 36652345.875 and 43267654.125 with 95% confidence. Note that n_real < 20 and the estimated variance may not be unbiased. Consider increasing the sample size.")
})

test_that("ct_cls works for example 8.3 and 8.4", {
  result <- ct_cls(C = 0.95, x_est = 8801, n_real = 25, N = 415, m = 6.04, M = 2500, sd_est = 25189, parameter = TRUE)
  expect_equal(round(result$t_est,0), 22002500)
  expect_equal(round(result$margin_of_error,0), 3961871)
  result <- ct_cls(C = 0.95, x_est = 8801, n_real = 25, N = 415, m = 6.04, sd_est = 21784, parameter = TRUE)
  expect_equal(round(result$t_est,0), 22060587)
  expect_equal(round(result$margin_of_error,0), 3435359)
})


