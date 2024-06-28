
function(C, e, p_exp, alloc = NULL, N)


test_that("Parameter ranges works ok", {
  expect_error(np_sts(C = 0.95, e = 0.1, p_exp = c(0.2, 0.3), N = c(220, 350)), "Parameter 'C' must be in the range 0 <= C <= 1")
  expect_error(np_sts(C = 0.95, e = 0.1, p_exp = c(0.2, 0.3), N = c(220, 350)), "Parameter 'C' must be in the range 0 <= C <= 1")
  expect_error(np_sts(C = 0.95, e = 0.1, p_exp = c(0.2, 0.3), N = c(220, 350)), "Parameter 'E' must be a positive integer")
  expect_error(np_sts(C = 0.95, e = 0.1, p_exp = c(0.2, 0.3), N = c(220, 350)), "All elements in 'sd_exp' must be positive integers")
  expect_error(np_sts(C = 0.95, e = 0.1, p_exp = c(0.1, 0.5, 0.5), alloc = c(0.3, 0.3, 0.4), N = c(150, 40, 110)), "All elements in 'alloc' must be greater than 0")
  expect_error(np_sts(C = 0.95, e = 0.1, p_exp = c(0.2, 0.3), N = c(220, 350)), "The sum of elements in 'alloc' must be equal to 1")
  expect_error(np_sts(C = 0.95, e = 0.1, p_exp = c(0.2, 0.3), N = c(220, 350)), "All elements in 'N' must be positive integers")
  expect_error(np_sts(C = 0.95, e = 0.1, p_exp = c(0.2, 0.3), N = c(220, 350)), "All elements in 'N' must be positive integers")
})

test_that("np_sts works", {
  result <- nx_sts(C = 0.95, E = 2, sd_exp = c(5, 15), N = c(220, 350), parameter = TRUE)
  expect_equal(result$n, 114)
  expect_equal(result$n_i, c(44, 70))
  result <- nx_sts(C = 0.95, E = 2, sd_exp = c(5, 15, 10), alloc = c(0.5, 0.2, 0.3), N = c(150, 40, 110), parameter = FALSE)
  expect_equal(result$n, 60)
  expect_equal(result$n_i, c(30, 12, 18))
})

