
test_that("Parameter ranges works ok", {
  expect_error(nx_sts(C = 1.1, E = 2, sd_exp = c(5, 15, 10), alloc = c(0.5, 0.2, 0.3), N = c(150, 40, 110), parameter = FALSE), "Parameter 'C' must be in the range 0 <= C <= 1")
  expect_error(nx_sts(C = -0.1, E = 2, sd_exp = c(5, 15, 10), alloc = c(0.5, 0.2, 0.3), N = c(150, 40, 110), parameter = FALSE), "Parameter 'C' must be in the range 0 <= C <= 1")
  expect_error(nx_sts(C = 0.95, E = -1, sd_exp = c(5, 15, 10), alloc = c(0.5, 0.2, 0.3), N = c(150, 40, 110), parameter = FALSE), "Parameter 'E' must be a positive integer")
  expect_error(nx_sts(C = 0.95, E = 2, sd_exp = c(-5, 15, 10), alloc = c(0.5, 0.2, 0.3), N = c(150, 40, 110), parameter = FALSE), "All elements in 'sd_exp' must be positive integers")
  expect_error(nx_sts(C = 0.95, E = 2, sd_exp = c(5, 15, 10), alloc = c(-0.5, 0.2, 0.3), N = c(150, 40, 110), parameter = FALSE), "All elements in 'alloc' must be greater than 0")
  expect_error(nx_sts(C = 0.95, E = 2, sd_exp = c(5, 15, 10), alloc = c(0.5, 0.2, 0.4), N = c(150, 40, 110), parameter = FALSE), "The sum of elements in 'alloc' must be equal to 1")
  expect_error(nx_sts(C = 0.95, E = 2, sd_exp = c(5, 15), alloc = c(0.5, 0.2, 0.3), N = c(150, 40, 110), parameter = FALSE), "'sd_exp', 'alloc', and 'N' must have the same length")
  expect_error(nx_sts(C = 0.95, E = 2, sd_exp = c(5, 15, 10), alloc = c(0.5, 0.2, 0.3), N = c(150.1, 40, 110), parameter = FALSE), "All elements in 'N' must be positive integers")
  expect_error(nx_sts(C = 0.95, E = 2, sd_exp = c(5, 15, 10), alloc = c(0.5, 0.2, 0.3), N = c(0, 40, 110), parameter = FALSE), "All elements in 'N' must be positive integers")
})

test_that("nx_sts works", {
  result <- nx_sts(C = 0.95, E = 2, sd_exp = c(5, 15), N = c(220, 350), parameter = TRUE)
  expect_equal(result$n, 114)
  expect_equal(result$n_i, c(44, 70))
  result <- nx_sts(C = 0.95, E = 2, sd_exp = c(5, 15, 10), alloc = c(0.5, 0.2, 0.3), N = c(150, 40, 110), parameter = FALSE)
  expect_equal(result$n, 60)
  expect_equal(result$n_i, c(30, 12, 18))
})

test_that("nx_sts works for example 5.5", {
  result <- nx_sts(C = 0.95, E = 2, sd_exp = c(5, 15, 10), alloc = c(1/3, 1/3, 1/3), N = c(155, 62, 93), parameter = TRUE)
  expect_equal(result$n, 57)
  expect_equal(result$n_i, c(19, 19, 19))
})
