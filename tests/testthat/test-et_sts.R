
test_that("Parameter ranges works ok", {
  expect_error(et_sts(C = 1.95, n_real = c(57, 29, 29), sd_est = c(5, 15, 10), alloc = c(1/2, 1/4, 1/4), N = c(150, 40, 110)), "Parameter 'C' must be in the range 0 <= C <= 1")
  expect_error(et_sts(C = -0.95, n_real = c(57, 29, 29), sd_est = c(5, 15, 10), alloc = c(1/2, 1/4, 1/4), N = c(150, 40, 110)), "Parameter 'C' must be in the range 0 <= C <= 1")
  expect_error(et_sts(C = 0.95, n_real = c(57.1, 29, 29), sd_est = c(5, 15, 10), alloc = c(1/2, 1/4, 1/4), N = c(150, 40, 110)), "All elements in 'n_real' must be positive integers")
  expect_error(et_sts(C = 0.95, n_real = c(-57, 29, 29), sd_est = c(5, 15, 10), alloc = c(1/2, 1/4, 1/4), N = c(150, 40, 110)), "All elements in 'n_real' must be positive integers")
  expect_error(et_sts(C = 0.95, n_real = c(57, 29, 29), sd_est = c(-5, 15, 10), alloc = c(1/2, 1/4, 1/4), N = c(150, 40, 110)), "All elements in 'sd_est' must be positive numbers")
  expect_error(et_sts(C = 0.95, n_real = c(57, 29, 29), sd_est = c(5, 15, 10), alloc = c(1.1, 1/4, 1/4), N = c(150, 40, 110)), "All elements in 'alloc' must be in the range 0 < alloc <= 1")
  expect_error(et_sts(C = 0.95, n_real = c(57, 29, 29), sd_est = c(5, 15, 10), alloc = c(-1/2, 1/4, 1/4), N = c(150, 40, 110)), "All elements in 'alloc' must be in the range 0 < alloc <= 1")
  expect_error(et_sts(C = 0.95, n_real = c(57, 29, 29), sd_est = c(5, 15, 10), alloc = c(1/4, 1/4, 1/4), N = c(150, 40, 110)), "The sum of elements in 'alloc' must be equal to 1")
  expect_error(et_sts(C = 0.95, n_real = c(57, 29, 29), sd_est = c(5, 15, 10), alloc = c(1/2, 1/4, 1/4), N = c(150.1, 40, 110)), "All elements in 'N' must be positive integers")
  expect_error(et_sts(C = 0.95, n_real = c(57, 29, 29), sd_est = c(5, 15, 10), alloc = c(1/2, 1/4, 1/4), N = c(-150, 40, 110)), "All elements in 'N' must be positive integers")
  expect_error(et_sts(C = 0.95, n_real = c(57, 29, 29), sd_est = c(5, 15, 10), alloc = c(1/2, 1/4, 1/4), N = c(40, 110)), "'n_real', 'sd_est', 'alloc', and 'N' must have the same length")
})

# Ref. to 5.6
test_that("et_sts works", {
  result <- et_sts(C = 0.95, n_real = c(57, 29, 29), sd_est = c(5, 15, 10), alloc = c(1/2, 1/4, 1/4), N = c(150, 40, 110), parameter = FALSE)
  expect_equal(round(result$E, 2), 399.36)
})

test_that("et_sts works for example 5.6", {
  result <- et_sts(C = 0.95, n_real = c(34, 34, 34), sd_est = c(5, 15, 10), alloc = c(1/3, 1/3, 1/3), N = c(155, 62, 93), parameter = TRUE)
  expect_equal(round(result$E, 0), 399)
})
