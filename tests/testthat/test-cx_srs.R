
test_that("Parameter ranges works ok", {
  expect_error(cx_srs(C = 1.95, sd_est = 200, x_est = 3500, n_real = 250, N = 5000), "Parameter 'C' must be in the range 0 <= C <= 1")
  expect_error(cx_srs(C = -0.95, sd_est = 200, x_est = 3500, n_real = 250, N = 5000), "Parameter 'C' must be in the range 0 <= C <= 1")
  expect_error(cx_srs(C = 0.95, sd_est = -200, x_est = 3500, n_real = 250, N = 5000), "Parameter 'sd_exp' must be a positive number")
  expect_error(cx_srs(C = 0.95, sd_est = 200, x_est = 3500, n_real = 250.1, N = 5000), "Parameter 'n_real' must be a positive integer")
  expect_error(cx_srs(C = 0.95, sd_est = 200, x_est = 3500, n_real = -250, N = 5000), "Parameter 'n_real' must be a positive integer")
  expect_error(cx_srs(C = 0.95, sd_est = 200, x_est = 3500, n_real = 250, N = 5000.1), "Parameter 'N' must be a positive integer or Inf")
  expect_error(cx_srs(C = 0.95, sd_est = 200, x_est = 3500, n_real = 250, N = -5000), "Parameter 'N' must be a positive integer or Inf")
})

test_that("cx_srs works", {
  result <- cx_srs(C = 0.95, sd_est = 200, x_est = 3500, n_real = 250)
  expect_equal(round(result$margin_of_error,4), 24.7918)
  result <- cx_srs(C = 0.95, sd_est = 200, x_est = 3500, n_real = 400)
  expect_equal(round(result$margin_of_error,5), 19.59964)
  result <- cx_srs(C = 0.95, sd_est = 200, x_est = 3500, n_real = 250, N = 5000, parameter = TRUE)
  expect_equal(round(result$margin_of_error,5), 24.16648)
})

test_that("cx_srs works for example 4.2", {
  result <- cx_srs(C = 0.95, sd_est = 21.1, x_est = 94.22, n_real = 200, N = 1000, parameter = TRUE)
  expect_equal(round(result$margin_of_error,3), 2.617)
})
