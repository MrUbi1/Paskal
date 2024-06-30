
test_that("Parameter ranges works ok", {
  expect_error(np_sts(C = 1.1, e = 0.1, p_exp = c(0.2, 0.3), N = c(220, 350)), "Parameter 'C' must be in the range 0 <= C <= 1")
  expect_error(np_sts(C = -0.1, e = 0.1, p_exp = c(0.2, 0.3), N = c(220, 350)), "Parameter 'C' must be in the range 0 <= C <= 1")
  expect_error(np_sts(C = 0.95, e = 1.1, p_exp = c(0.2, 0.3), N = c(220, 350)), "Parameter 'e' must be in the range 0 <= e <= 1")
  expect_error(np_sts(C = 0.95, e = -0.1, p_exp = c(0.2, 0.3), N = c(220, 350)), "Parameter 'e' must be in the range 0 <= e <= 1")
  expect_error(np_sts(C = 0.95, e = 0.1, p_exp = c(-0.2, 0.3), N = c(220, 350)), "All elements in 'p_exp' must be in the range 0 <= p_exp <= 1")
  expect_error(np_sts(C = 0.95, e = 0.1, p_exp = c(1.2, 0.3), N = c(220, 350)), "All elements in 'p_exp' must be in the range 0 <= p_exp <= 1")
  expect_error(np_sts(C = 0.95, e = 0.1, p_exp = c(0.1, 0.5, 0.5), alloc = c(-0.3, 0.3, 0.4), N = c(150, 40, 110)), "All elements in 'alloc' must be in the range 0 <= alloc <= 1")
  expect_error(np_sts(C = 0.95, e = 0.1, p_exp = c(0.1, 0.5, 0.5), alloc = c(1.2, -0.1, -0.1), N = c(150, 40, 110)), "All elements in 'alloc' must be in the range 0 <= alloc <= 1")
  expect_error(np_sts(C = 0.95, e = 0.1, p_exp = c(0.1, 0.5, 0.5), alloc = c(0.4, 0.3, 0.4), N = c(150, 40, 110)), "The sum of elements in 'alloc' must be equal to 1")
  expect_error(np_sts(C = 0.95, e = 0.1, p_exp = c(0.1, 0.5, 0.5), alloc = c(0.3, 0.3, 0.4), N = c(150, 40)), "'p_exp', 'alloc', and 'N' must have the same length")
  expect_error(np_sts(C = 0.95, e = 0.1, p_exp = c(0.2, 0.3), N = c(220.1, 350)), "All elements in 'N' must be positive integers")
  expect_error(np_sts(C = 0.95, e = 0.1, p_exp = c(0.2, 0.3), alloc = c(0.5, 0.5), N = c(-220, 350)), "All elements in 'N' must be positive integers")
})

test_that("np_sts works", {
  result <- np_sts(C = 0.95, e = 0.1, p_exp = c(0.2, 0.3), N = c(220, 350))
  expect_equal(result$n, 66)
  expect_equal(result$n_i, c(26, 40))
  result <- np_sts(C = 0.95, e = 0.1, p_exp = c(0.1, 0.5, 0.5), alloc = c(0.3, 0.3, 0.4), N = c(150, 40, 110))
  expect_equal(result$n, 56)
  expect_equal(result$n_i, c(17, 17, 22))
})

