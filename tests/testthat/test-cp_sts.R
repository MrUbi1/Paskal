
test_that("Parameter ranges works ok", {
  expect_error(cp_sts(C = 1.95, n_real = c(100, 150, 200), p_est = c(0.3, 0.5, 0.7), N = c(200, 250, 300)), "Parameter 'C' must be in the range 0 <= C <= 1")
  expect_error(cp_sts(C = -0.95, n_real = c(100, 150, 200), p_est = c(0.3, 0.5, 0.7), N = c(200, 250, 300)), "Parameter 'C' must be in the range 0 <= C <= 1")
  expect_error(cp_sts(C = 0.95, n_real = c(100.1, 150, 200), p_est = c(0.3, 0.5, 0.7), N = c(200, 250, 300)), "All elements in 'n_real' must be positive integers")
  expect_error(cp_sts(C = 0.95, n_real = c(-100, 150, 200), p_est = c(0.3, 0.5, 0.7), N = c(200, 250, 300)), "All elements in 'n_real' must be positive integers")
  expect_error(cp_sts(C = 0.95, n_real = c(100, 150, 200), p_est = c(1.3, 0.5, 0.7), N = c(200, 250, 300)), "All elements in 'p_est' must be in the range 0 <= p_est <= 1")
  expect_error(cp_sts(C = 0.95, n_real = c(100, 150, 200), p_est = c(-0.3, 0.5, 0.7), N = c(200, 250, 300)), "All elements in 'p_est' must be in the range 0 <= p_est <= 1")
  expect_error(cp_sts(C = 0.95, n_real = c(100, 150, 200), p_est = c(0.3, 0.5, 0.7), N = c(200.1, 250, 300)), "All elements in 'N' must be positive integers")
  expect_error(cp_sts(C = 0.95, n_real = c(100, 150, 200), p_est = c(0.3, 0.5, 0.7), N = c(-200, 250, 300)), "All elements in 'N' must be positive integers")
  expect_error(cp_sts(C = 0.95, n_real = c(100, 150, 200), p_est = c(0.3, 0.5, 0.7), N = c(200, 250, 300, 1)), "'p_est', 'n_real', and 'N' must have the same length")
})

test_that("cp_sts works", {
  result <- cp_sts(C = 0.95, n_real = c(100, 150, 200), p_est = c(0.3, 0.5, 0.7), N = c(200, 250, 300))
  expect_equal(round(result$global_p_est, 4), 0.5267)
  expect_equal(round(result$margin_of_error, 5), 0.02815)
})

test_that("cp_sts works for example 5.12", {
  result <- cp_sts(C = 0.95, n_real = c(20, 8, 12), p_est = c(0.8, 0.25, 0.5), N = c(155, 62, 93))
  expect_equal(round(result$global_p_est, 1), 0.6)
  expect_equal(round(result$margin_of_error, 5), 0.13218)
})
