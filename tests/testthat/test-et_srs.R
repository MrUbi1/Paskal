
test_that("Parameter ranges works ok", {
  expect_error(et_srs(C = 1.95, n_real = 6, sd_est = 2.1, N = 1200, parameter = TRUE), "Parameter 'C' must be in the range 0 <= C <= 1")
  expect_error(et_srs(C = -0.95, n_real = 6, sd_est = 2.1, N = 1200, parameter = TRUE), "Parameter 'C' must be in the range 0 <= C <= 1")
  expect_error(et_srs(C = 0.95, n_real = 6.1, sd_est = 2.1, N = 1200, parameter = TRUE), "Parameter 'n_real' must be a positive integer")
  expect_error(et_srs(C = 0.95, n_real = -6, sd_est = 2.1, N = 1200, parameter = TRUE), "Parameter 'n_real' must be a positive integer")
  expect_error(et_srs(C = 0.95, n_real = 6, sd_est = -2.1, N = 1200, parameter = TRUE), "Parameter 'sd_est' must be a positive number")
  expect_error(et_srs(C = 0.95, n_real = 6, sd_est = 2.1, N = 1200.1, parameter = TRUE), "Parameter 'N' must be a positive integer")
  expect_error(et_srs(C = 0.95, n_real = 6, sd_est = 2.1, N = -1200, parameter = TRUE), "Parameter 'N' must be a positive integer")
})

test_that("et_srs works", {
  result <- et_srs(C = 0.95, n_real = 87, sd_est = 4.1, N = 1200)
  expect_equal(round(result$E, 2), 997.08)
  result <- et_srs(C = 0.95, n_real = 23, sd_est = 4.1, N = 1200)
  expect_equal(round(result$E, 3), 1994.188)
  result <- et_srs(C = 0.95, n_real = 6, sd_est = 2.1, N = 1200, parameter = TRUE)
  expect_equal(round(result$E, 3), 2012.174)
})

test_that("et_srs works for reverse of example 4.6", {
  result <- et_srs(C = 0.95, n_real = 122, sd_est = 6, N = 1000, parameter = TRUE)
  expect_equal(round(result$E, 0), 998)
})


