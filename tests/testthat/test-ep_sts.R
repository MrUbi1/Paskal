
test_that("Parameter ranges works ok", {
  expect_error(ep_sts(C = 1.95, n_real = c(66, 9, 9), p_est = c(0.2, 0.5, 0.7), alloc = c(0.8, 0.1, 0.1), N = c(1400, 400, 200)), "Parameter 'C' must be in the range 0 <= C <= 1")
  expect_error(ep_sts(C = -0.95, n_real = c(66, 9, 9), p_est = c(0.2, 0.5, 0.7), alloc = c(0.8, 0.1, 0.1), N = c(1400, 400, 200)), "Parameter 'C' must be in the range 0 <= C <= 1")
  expect_error(ep_sts(C = 0.95, n_real = c(66.1, 9, 9), p_est = c(0.2, 0.5, 0.7), alloc = c(0.8, 0.1, 0.1), N = c(1400, 400, 200)), "All elements in 'n_real' must be positive integers")
  expect_error(ep_sts(C = 0.95, n_real = c(-66, 9, 9), p_est = c(0.2, 0.5, 0.7), alloc = c(0.8, 0.1, 0.1), N = c(1400, 400, 200)), "All elements in 'n_real' must be positive integers")
  expect_error(ep_sts(C = 0.95, n_real = c(66, 9, 9), p_est = c(1.2, 0.5, 0.7), alloc = c(0.8, 0.1, 0.1), N = c(1400, 400, 200)), "All elements in 'p_est' must be in the range 0 <= p_est <= 1")
  expect_error(ep_sts(C = 0.95, n_real = c(66, 9, 9), p_est = c(-0.2, 0.5, 0.7), alloc = c(0.8, 0.1, 0.1), N = c(1400, 400, 200)), "All elements in 'p_est' must be in the range 0 <= p_est <= 1")
  expect_error(ep_sts(C = 0.95, n_real = c(66, 9, 9), p_est = c(0.2, 0.5, 0.7), alloc = c(1.8, 0.1, 0.1), N = c(1400, 400, 200)), "All elements in 'alloc' must be in the range 0 < alloc <= 1")
  expect_error(ep_sts(C = 0.95, n_real = c(66, 9, 9), p_est = c(0.2, 0.5, 0.7), alloc = c(-0.8, 0.1, 0.1), N = c(1400, 400, 200)), "All elements in 'alloc' must be in the range 0 < alloc <= 1")
  expect_error(ep_sts(C = 0.95, n_real = c(66, 9, 9), p_est = c(0.2, 0.5, 0.7), alloc = c(0.9, 0.1, 0.1), N = c(1400, 400, 200)), "The sum of elements in 'alloc' must be equal to 1")
  expect_error(ep_sts(C = 0.95, n_real = c(66, 9, 9), p_est = c(0.2, 0.5, 0.7), alloc = c(0.8, 0.1, 0.1), N = c(1400.1, 400, 200)), "All elements in 'N' must be positive integers")
  expect_error(ep_sts(C = 0.95, n_real = c(66, 9, 9), p_est = c(0.2, 0.5, 0.7), alloc = c(0.8, 0.1, 0.1), N = c(-1400, 400, 200)), "All elements in 'N' must be positive integers")
  expect_error(ep_sts(C = 0.95, n_real = c(66, 9, 9), p_est = c(0.2, 0.5, 0.7), alloc = c(0.8, 0.1, 0.1), N = c(400, 200)), "'n_real', 'p_est', 'alloc', and 'N' must have the same length")
})

#  Ref. to 5.15
test_that("ep_sts works", {
  result <- ep_sts(C = 0.95, n_real = c(66, 9, 9), p_est = c(0.2, 0.5, 0.7), alloc = c(0.8, 0.1, 0.1), N = c(1400, 400, 200))
  expect_equal(round(result$e, 4), 0.0983)
})

test_that("ep_sts works for reverse of example 5.16", {
  result <- ep_sts(C = 0.95, n_real = c(36, 15, 22), p_est = c(0.4, 0.4, 0.4), N = c(155, 62, 93))
  expect_equal(round(result$e, 1), 0.1)
})
