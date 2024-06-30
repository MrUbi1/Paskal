
test_that("Parameter ranges works ok", {
  expect_error(ex_sts(C = 1.95, n_real = c(48, 14, 7), sd_est = c(0.2, 0.5, 0.7), alloc =c(0.7, 0.2, 0.1), N = c(1400, 400, 200)), "Parameter 'C' must be in the range 0 <= C <= 1")
  expect_error(ex_sts(C = -0.95, n_real = c(48, 14, 7), sd_est = c(0.2, 0.5, 0.7), alloc =c(0.7, 0.2, 0.1), N = c(1400, 400, 200)), "Parameter 'C' must be in the range 0 <= C <= 1")
  expect_error(ex_sts(C = 0.95, n_real = c(48.1, 14, 7), sd_est = c(0.2, 0.5, 0.7), alloc =c(0.7, 0.2, 0.1), N = c(1400, 400, 200)), "All elements in 'n_real' must be positive integers")
  expect_error(ex_sts(C = 0.95, n_real = c(-48, 14, 7), sd_est = c(0.2, 0.5, 0.7), alloc =c(0.7, 0.2, 0.1), N = c(1400, 400, 200)), "All elements in 'n_real' must be positive integers")
  expect_error(ex_sts(C = 0.95, n_real = c(48, 14, 7), sd_est = c(0.2, -0.5, 0.7), alloc =c(0.7, 0.2, 0.1), N = c(1400, 400, 200)), "All elements in 'sd_est' must be positive numbers")
  expect_error(ex_sts(C = 0.95, n_real = c(48, 14, 7), sd_est = c(0.2, 0.5, 0.7), alloc =c(0.7, 1.2, 0.1), N = c(1400, 400, 200)), "All elements in 'alloc' must be in the range 0 < alloc <= 1")
  expect_error(ex_sts(C = 0.95, n_real = c(48, 14, 7), sd_est = c(0.2, 0.5, 0.7), alloc =c(0.7, -0.2, 0.1), N = c(1400, 400, 200)), "All elements in 'alloc' must be in the range 0 < alloc <= 1")
  expect_error(ex_sts(C = 0.95, n_real = c(48, 14, 7), sd_est = c(0.2, 0.5, 0.7), alloc =c(0.7, 0.3, 0.1), N = c(1400, 400, 200)), "The sum of elements in 'alloc' must be equal to 1")
  expect_error(ex_sts(C = 0.95, n_real = c(48, 14, 7), sd_est = c(0.2, 0.5, 0.7), alloc =c(0.7, 0.2, 0.1), N = c(1400.1, 400, 200)), "All elements in 'N' must be positive integers")
  expect_error(ex_sts(C = 0.95, n_real = c(48, 14, 7), sd_est = c(0.2, 0.5, 0.7), alloc =c(0.7, 0.2, 0.1), N = c(-1400, 400, 200)), "All elements in 'N' must be positive integers")
  expect_error(ex_sts(C = 0.95, n_real = c(48, 14, 7), sd_est = c(0.2, 0.5, 0.7), alloc =c(0.7, 0.2, 0.1), N = c(400, 200)), "'n_real', 'sd_est', 'alloc', and 'N' must have the same length")
})

test_that("ex_sts works", {
  result <- ex_sts(C = 0.95, n_real = c(48, 14, 7), sd_est = c(0.2, 0.5, 0.7), alloc =c(0.7, 0.2, 0.1), N = c(1400, 400, 200), parameter = TRUE)
  expect_equal(round(result$E, 4), 0.0826)

})


