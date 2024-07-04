
test_that("Parameter ranges works ok", {
  expect_error(ct_srs(C = 1.95, t_est = 3500, sd_est = 4.1, n_real = 87, N = 1200), "Parameter 'C' must be in the range 0 <= C <= 1")
  expect_error(ct_srs(C = -0.95, t_est = 3500, sd_est = 4.1, n_real = 87, N = 1200), "Parameter 'C' must be in the range 0 <= C <= 1")
  expect_error(ct_srs(C = 0.95, t_est = 3500, sd_est = -4.1, n_real = 87, N = 1200), "Parameter 'sd_est' must be a positive number")
  expect_error(ct_srs(C = 0.95, t_est = 3500, sd_est = 4.1, n_real = 87.1, N = 1200), "Parameter 'n_real' must be a positive integer")
  expect_error(ct_srs(C = 0.95, t_est = 3500, sd_est = 4.1, n_real = -87, N = 1200), "Parameter 'n_real' must be a positive integer")
  expect_error(ct_srs(C = 0.95, t_est = 3500, sd_est = 4.1, n_real = 87, N = 1200.1), "Parameter 'N' must be a positive integer or Inf")
  expect_error(ct_srs(C = 0.95, t_est = 3500, sd_est = 4.1, n_real = 87, N = -1200), "Parameter 'N' must be a positive integer or Inf")
})

test_that("ct_srs works", {
  result <- ct_srs(C = 0.95, t_est = 3500, sd_est = 4.1, n_real = 87, N = 1200)
  expect_equal(round(result$margin_of_error,4), 997.0796)
})

test_that("ct_srs works for example 4.4", {
  result <- ct_srs(C = 0.95, t_est = 7732.5, sd_est = 1.5, n_real = 50, N = 750, parameter = TRUE)
  expect_equal(round(result$margin_of_error,3), 301.456)
})
