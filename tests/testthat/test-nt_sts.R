
test_that("Parameter ranges works ok", {
  expect_error(nx_sts(C = 1.1, E = 2, sd_exp = c(5, 15, 10), alloc = c(0.5, 0.2, 0.3), N = c(150, 40, 110), parameter = FALSE), "Parameter 'C' must be in the range 0 <= C <= 1")
  expect_error(nx_sts(C = -0.1, E = 2, sd_exp = c(5, 15, 10), alloc = c(0.5, 0.2, 0.3), N = c(150, 40, 110), parameter = FALSE), "Parameter 'C' must be in the range 0 <= C <= 1")
  expect_error(nx_sts(C = 0.95, E = -1, sd_exp = c(5, 15, 10), alloc = c(0.5, 0.2, 0.3), N = c(150, 40, 110), parameter = FALSE), "Parameter 'E' must be a positive integer")
  expect_error(nx_sts(C = 0.95, E = 2, sd_exp = c(-5, 15, 10), alloc = c(0.5, 0.2, 0.3), N = c(150, 40, 110), parameter = FALSE), "All elements in 'sd_exp' must be positive integers")
  expect_error(nx_sts(C = 0.95, E = 2, sd_exp = c(5, 15, 10), alloc = c(-0.5, 0.2, 0.3), N = c(150, 40, 110), parameter = FALSE), "All elements in 'alloc' must be greater than 0")
  expect_error(nx_sts(C = 0.95, E = 2, sd_exp = c(5, 15, 10), alloc = c(0.5, 0.2, 0.4), N = c(150, 40, 110), parameter = FALSE), "The sum of elements in 'alloc' must be equal to 1")
  expect_error(nx_sts(C = 0.95, E = 2, sd_exp = c(5, 15, 10), alloc = c(0.5, 0.2, 0.3), N = c(150.1, 40, 110), parameter = FALSE), "All elements in 'N' must be positive integers")
  expect_error(nx_sts(C = 0.95, E = 2, sd_exp = c(5, 15, 10), alloc = c(0.5, 0.2, 0.3), N = c(0, 40, 110), parameter = FALSE), "All elements in 'N' must be positive integers")
})

test_that("nt_sts works", {
  result <- nt_sts(C = 0.95, E = 200, sd_exp = c(5, 15), alloc = c(1/2, 1/2), N = c(155, 62), parameter = TRUE)
  expect_equal(result$n, 104)
  expect_equal(result$n_i, c(52, 52))
  result <- nt_sts(C = 0.95, E = 400, sd_exp = c(5, 15, 10), alloc = c(1/2, 1/4, 1/4), N = c(150, 40, 110), parameter = FALSE)
  expect_equal(result$n, 115)
  expect_equal(result$n_i, c(57, 29, 29))
})
