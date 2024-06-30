
test_that("Parameter ranges works ok", {
  expect_error(nx_cls(C = 1.1, E = 650, sd_exp = 15000, m = 8, N = 500), "Parameter 'C' must be in the range 0 <= C <= 1")
  expect_error(nx_cls(C = -0.95, E = 650, sd_exp = 15000, m = 8, N = 500), "Parameter 'C' must be in the range 0 <= C <= 1")
  expect_error(nx_cls(C = 0.95, E = -650, sd_exp = 15000, m = 8, N = 500), "Parameter 'E' must be a positive number")
  expect_error(nx_cls(C = 0.95, E = 650, sd_exp = -15000, m = 8, N = 500), "Parameter 'sd_exp' must be a positive number")
  expect_error(nx_cls(C = 0.95, E = 650, sd_exp = 15000, m = 0, N = 500), "Parameter 'm' must be a positive number")
  expect_error(nx_cls(C = 0.95, E = 650, sd_exp = 15000, m = 8, N = 500.1), "Parameter 'N' must be a positive integer or Inf")
  expect_error(nx_cls(C = 0.95, E = 650, sd_exp = 15000, m = 8, N = -500), "Parameter 'N' must be a positive integer or Inf")
})

test_that("nx_cls works", {
  result <- nx_cls(C = 0.95, E = 650, sd_exp = 15000, m = 8, N = 500)
  expect_equal(result$n, 31)
})
