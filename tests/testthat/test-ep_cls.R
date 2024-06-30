
test_that("Parameter ranges works ok", {
  expect_error(ep_cls(C = 1.95, n_real = 31, sd_est = 0.726, m = 6.04, N = 415, parameter = TRUE), "Parameter 'C' must be in the range 0 <= C <= 1")
  expect_error(ep_cls(C = -0.95, n_real = 31, sd_est = 0.726, m = 6.04, N = 415, parameter = TRUE), "Parameter 'C' must be in the range 0 <= C <= 1")
  expect_error(ep_cls(C = 0.95, n_real = 31.1, sd_est = 0.726, m = 6.04, N = 415, parameter = TRUE), "Parameter 'n_real' must be a positive integer")
  expect_error(ep_cls(C = 0.95, n_real = -31, sd_est = 0.726, m = 6.04, N = 415, parameter = TRUE), "Parameter 'n_real' must be a positive integer")
  expect_error(ep_cls(C = 0.95, n_real = 31, sd_est = -0.726, m = 6.04, N = 415, parameter = TRUE), "Parameter 'sd_est' must be a positive number")
  expect_error(ep_cls(C = 0.95, n_real = 31, sd_est = 0.726, m = -6.04, N = 415, parameter = TRUE), "Parameter 'm' must be a positive number")
  expect_error(ep_cls(C = 0.95, n_real = 31, sd_est = 0.726, m = 6.04, N = 415.1, parameter = TRUE), "Parameter 'N' must be a positive integer or Inf")
  expect_error(ep_cls(C = 0.95, n_real = 31, sd_est = 0.726, m = 6.04, N = -415, parameter = TRUE), "Parameter 'N' must be a positive integer or Inf")
})

test_that("ep_cls works", {
  result <- ep_cls(C = 0.95, n_real = 31, sd_est = 0.726, m = 6.04, N = 415, parameter = TRUE)
  expect_equal(round(result$e, 2), 0.04)
})

