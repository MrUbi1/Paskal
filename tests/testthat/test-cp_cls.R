
test_that("Parameter ranges works ok", {
  expect_error(cp_cls(C = 1.95, p_est = 0.48, n_real = 25, N = 415, m = 8, sd_est = 0.73, parameter = TRUE), "Parameter 'C' must be in the range 0 <= C <= 1")
  expect_error(cp_cls(C = -0.95, p_est = 0.48, n_real = 25, N = 415, m = 8, sd_est = 0.73, parameter = TRUE), "Parameter 'C' must be in the range 0 <= C <= 1")
  expect_error(cp_cls(C = 0.95, p_est = 0.48, n_real = 25, N = 415, m = 8, sd_est = -0.73, parameter = TRUE), "Parameter 'sd_est' must be a positive number")
  expect_error(cp_cls(C = 0.95, p_est = 0.48, n_real = 25.1, N = 415, m = 8, sd_est = 0.73, parameter = TRUE), "Parameter 'n_real' must be a positive integer")
  expect_error(cp_cls(C = 0.95, p_est = 0.48, n_real = -25, N = 415, m = 8, sd_est = 0.73, parameter = TRUE), "Parameter 'n_real' must be a positive integer")
  expect_error(cp_cls(C = 0.95, p_est = 0.48, n_real = 25, N = 415, m = -8, sd_est = 0.73, parameter = TRUE), "Parameter 'm' must be a positive number")
  expect_error(cp_cls(C = 0.95, p_est = 0.48, n_real = 25, N = 415.1, m = 8, sd_est = 0.73, parameter = TRUE), "Parameter 'N' must be a positive integer or Inf")
  expect_error(cp_cls(C = 0.95, p_est = 0.48, n_real = 25, N = -415, m = 8, sd_est = 0.73, parameter = TRUE), "Parameter 'N' must be a positive integer or Inf")
})

test_that("cp_cls works", {
  result <- cp_cls(C = 0.95, p_est = 0.48, n_real = 25, N = 415, m = 8, sd_est = 0.73, parameter = TRUE)
  expect_equal(round(result$p_est, 2), 0.48)
  expect_equal(round(result$margin_of_error,5), 0.03468)
  result <- cp_cls(C = 0.95, p_est = 0.48, n_real = 19, N = 500, m = 8, sd_est = 0.33, parameter = TRUE)
  expect_equal(round(result$p_est, 2), 0.48)
  expect_equal(round(result$margin_of_error,5), 0.01819)
  expect_equal(result$inference, "The population mean is between 0.462 and 0.498 with 95% confidence. Note that n_real < 20 and the estimated variance may not be unbiased. Consider increasing the sample size.")
})
