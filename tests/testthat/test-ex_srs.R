
test_that("Parameter ranges works ok", {
  expect_error(ex_srs(C = 1.95, n_real = 140, sd_est = 800, N = 2000, parameter = TRUE), "Parameter 'C' must be in the range 0 <= C <= 1")
  expect_error(ex_srs(C = -0.95, n_real = 140, sd_est = 800, N = 2000, parameter = TRUE), "Parameter 'C' must be in the range 0 <= C <= 1")
  expect_error(ex_srs(C = 0.95, n_real = 140.1, sd_est = 800, N = 2000, parameter = TRUE), "Parameter 'n_real' must be a positive integer")
  expect_error(ex_srs(C = 0.95, n_real = -140, sd_est = 800, N = 2000, parameter = TRUE), "Parameter 'n_real' must be a positive integer")
  expect_error(ex_srs(C = 0.95, n_real = 140, sd_est = -800, N = 2000, parameter = TRUE), "Parameter 'sd_est' must be a positive number")
  expect_error(ex_srs(C = 0.95, n_real = 140, sd_est = 800, N = 2000.1, parameter = TRUE), "Parameter 'N' must be a positive integer or Inf")
  expect_error(ex_srs(C = 0.95, n_real = 140, sd_est = 800, N = -2000, parameter = TRUE), "Parameter 'N' must be a positive integer or Inf")
})

test_that("ex_srs works", {
  result <- ex_srs(C = 0.95, n_real = 140, sd_est = 600)
  expect_equal(round(result$E, 4), 99.3883)
  result <- ex_srs(C = 0.95, n_real = 140, sd_est = 800)
  expect_equal(round(result$E, 4), 132.5177)
  result <- ex_srs(C = 0.95, n_real = 140, sd_est = 800, N = 2000, parameter = TRUE)
  expect_equal(round(result$E, 4), 127.8275)
})
