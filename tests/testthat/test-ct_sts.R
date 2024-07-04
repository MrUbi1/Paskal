
test_that("Parameter ranges works ok", {
  expect_error(ct_sts(C = 1.95, n_real = c(100, 150, 200), x_est = c(0.3, 0.5, 0.7), sd_est = c(0.5, 0.6, 0.4), N = c(200, 250, 300)), "Parameter 'C' must be in the range 0 <= C <= 1")
  expect_error(ct_sts(C = -0.95, n_real = c(100, 150, 200), x_est = c(0.3, 0.5, 0.7), sd_est = c(0.5, 0.6, 0.4), N = c(200, 250, 300)), "Parameter 'C' must be in the range 0 <= C <= 1")
  expect_error(ct_sts(C = 0.95, n_real = c(100.1, 150, 200), x_est = c(0.3, 0.5, 0.7), sd_est = c(0.5, 0.6, 0.4), N = c(200, 250, 300)), "All elements in 'n_real' must be positive integers")
  expect_error(ct_sts(C = 0.95, n_real = c(-100, 150, 200), x_est = c(0.3, 0.5, 0.7), sd_est = c(0.5, 0.6, 0.4), N = c(200, 250, 300)), "All elements in 'n_real' must be positive integers")
  expect_error(ct_sts(C = 0.95, n_real = c(100, 150, 200), x_est = c(0.3, 0.5, 0.7), sd_est = c(-0.5, 0.6, 0.4), N = c(200, 250, 300)), "All elements in 'sd_est' must be positive numbers")
  expect_error(ct_sts(C = 0.95, n_real = c(100, 150, 200), x_est = c(0.3, 0.5, 0.7), sd_est = c(0.5, 0.6, 0.4), N = c(200.1, 250, 300)), "All elements in 'N' must be positive integers")
  expect_error(ct_sts(C = 0.95, n_real = c(100, 150, 200), x_est = c(0.3, 0.5, 0.7), sd_est = c(0.5, 0.6, 0.4), N = c(-200, 250, 300)), "All elements in 'N' must be positive integers")
  expect_error(ct_sts(C = 0.95, n_real = c(100, 150, 200), x_est = c(0.3, 0.5, 0.7), sd_est = c(0.5, 0.6, 0.4), N = c(200, 250, 300, 1)), "'x_est', 'n_real', 'sd_est' and 'N' must have the same length")
})

test_that("ct_sts works", {
  result <- ct_sts(C = 0.95, n_real = c(100, 150, 200), x_est = c(0.3, 0.5, 0.7), sd_est = c(0.5, 0.6, 0.4), N = c(200, 250, 300))
  expect_equal(round(result$global_t_est, 0), 395)
  expect_equal(round(result$margin_of_error, 5), 22.74941)
})

test_that("ct_sts works for example 5.3", {
  result <- ct_sts(C = 0.95, n_real = c(20, 8, 12), x_est = c(33.9, 25.12, 19), sd_est = c(5.95, 15.25, 9.36), N = c(155, 62, 93), parameter = TRUE)
  expect_equal(round(result$global_t_est, 2), 8578.94)
  expect_equal(round(result$margin_of_error, 4), 852.8979)
})
