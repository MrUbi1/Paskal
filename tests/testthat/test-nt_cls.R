
test_that("Parameter ranges works ok", {
  expect_error(nt_cls(C = 1.95, E = 500000, sd_exp = 5000, N = 400), "Parameter 'C' must be in the range 0 <= C <= 1")
  expect_error(nt_cls(C = -0.95, E = 500000, sd_exp = 5000, N = 400), "Parameter 'C' must be in the range 0 <= C <= 1")
  expect_error(nt_cls(C = 0.95, E = -500000, sd_exp = 5000, N = 400), "Parameter 'E' must be a positive number")
  expect_error(nt_cls(C = 0.95, E = 500000, sd_exp = -5000, N = 400), "Parameter 'sd_exp' must be a positive number")
  expect_error(nt_cls(C = 0.95, E = 500000, sd_exp = 5000, N = 400.1), "Parameter 'N' must be a positive integer or Inf")
  expect_error(nt_cls(C = 0.95, E = 500000, sd_exp = 5000, N = -400), "Parameter 'N' must be a positive integer or Inf")
})

test_that("nt_cls works", {
  result <- nt_cls(C = 0.95, E = 500000, sd_exp = 5000, N = 400)
  expect_equal(result$n, 54)
})
