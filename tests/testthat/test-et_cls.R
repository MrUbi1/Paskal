
test_that("Parameter ranges works ok", {
  expect_error(et_cls(C = 1.95, n_real = 213, sd_est = 25189, N = 415), "Parameter 'C' must be in the range 0 <= C <= 1")
  expect_error(et_cls(C = -0.95, n_real = 213, sd_est = 25189, N = 415), "Parameter 'C' must be in the range 0 <= C <= 1")
  expect_error(et_cls(C = 0.95, n_real = 213.1, sd_est = 25189, N = 415), "Parameter 'n_real' must be a positive integer")
  expect_error(et_cls(C = 0.95, n_real = -213, sd_est = 25189, N = 415), "Parameter 'n_real' must be a positive integer")
  expect_error(et_cls(C = 0.95, n_real = 213, sd_est = -25189, N = 415), "Parameter 'sd_est' must be a positive number")
  expect_error(et_cls(C = 0.95, n_real = 213, sd_est = 25189, N = 415.1), "Parameter 'N' must be a positive integer or Inf")
  expect_error(et_cls(C = 0.95, n_real = 213, sd_est = 25189, N = -415), "Parameter 'N' must be a positive integer or Inf")
})

# Ref. to 8.13 / 8.15
test_that("et_cls works", {
  result <- et_cls(C = 0.95, n_real = 213, sd_est = 25189, N = 415, parameter = TRUE)
  expect_equal(round(result$E, 0), 979420)
})

test_that("et_cls works for reverse of example 8.7", {
  result <- et_cls(C = 0.95, n_real = 209, sd_est = 25189, N = 415, parameter = TRUE)
  expect_equal(round(result$E, 0), 998490)
})
