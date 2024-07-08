
test_that("Parameter ranges works ok", {
  expect_error(cx_sts(C = 1.95, n_real = c(100, 150, 200), x_est = c(3.2, 3.5, 3.7), sd_est = c(0.5, 0.6, 0.4), N = c(200, 250, 300)), "Parameter 'C' must be in the range 0 <= C <= 1")
  expect_error(cx_sts(C = -0.95, n_real = c(100, 150, 200), x_est = c(3.2, 3.5, 3.7), sd_est = c(0.5, 0.6, 0.4), N = c(200, 250, 300)), "Parameter 'C' must be in the range 0 <= C <= 1")
  expect_error(cx_sts(C = 0.95, n_real = c(100.1, 150, 200), x_est = c(3.2, 3.5, 3.7), sd_est = c(0.5, 0.6, 0.4), N = c(200, 250, 300)), "All elements in 'n_real' must be positive integers")
  expect_error(cx_sts(C = 0.95, n_real = c(-100, 150, 200), x_est = c(3.2, 3.5, 3.7), sd_est = c(0.5, 0.6, 0.4), N = c(200, 250, 300)), "All elements in 'n_real' must be positive integers")
  expect_error(cx_sts(C = 0.95, n_real = c(100, 150, 200), x_est = c(3.2, 3.5, 3.7), sd_est = c(-0.5, 0.6, 0.4), N = c(200, 250, 300)), "All elements in 'sd_est' must be positive numbers")
  expect_error(cx_sts(C = 0.95, n_real = c(100, 150, 200), x_est = c(3.2, 3.5, 3.7), sd_est = c(0.5, 0.6, 0.4), N = c(200.1, 250, 300)), "All elements in 'N' must be positive integers")
  expect_error(cx_sts(C = 0.95, n_real = c(100, 150, 200), x_est = c(3.2, 3.5, 3.7), sd_est = c(0.5, 0.6, 0.4), N = c(-200, 250, 300)), "All elements in 'N' must be positive integers")
  expect_error(cx_sts(C = 0.95, n_real = c(100, 150, 200), x_est = c(3.2, 3.5, 3.7), sd_est = c(0.5, 0.6, 0.4), N = c(200, 250, 300, 1)), "'x_est', 'n_real', 'sd_est' and 'N' must have the same length")
})

# Ref. to 5.2
test_that("cx_sts works", {
  result <- cx_sts(C = 0.95, n_real = c(100, 150, 200), x_est = c(3.2, 3.5, 3.7), sd_est = c(0.5, 0.6, 0.4), N = c(200, 250, 300), parameter = TRUE)
  expect_equal(round(result$global_x_est, 1), 3.5)
  expect_equal(round(result$margin_of_error, 5), 0.03025)
})

test_that("cx_sts works for example 5.2", {
  result <- cx_sts(C = 0.95, n_real = c(20, 8, 12), x_est = c(33.9, 25.12, 19), sd_est = c(5.95, 15.25, 9.36), N = c(155, 62, 93), parameter = TRUE)
  expect_equal(round(result$global_x_est, 1), 27.7)
  expect_equal(round(result$margin_of_error, 5), 2.75128)
})
