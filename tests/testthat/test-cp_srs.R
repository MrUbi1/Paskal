
test_that("Parameter ranges works ok", {
  expect_error(cp_srs(C = 1.95, n_real = 250, p_est = 0.4, N = 5000), "Parameter 'C' must be in the range 0 <= C <= 1")
  expect_error(cp_srs(C = -0.95, n_real = 250, p_est = 0.4, N = 5000), "Parameter 'C' must be in the range 0 <= C <= 1")
  expect_error(cp_srs(C = 0.95, n_real = 250.1, p_est = 0.4, N = 5000), "Parameter 'n_real' must be a positive integer")
  expect_error(cp_srs(C = 0.95, n_real = -250, p_est = 0.4, N = 5000), "Parameter 'n_real' must be a positive integer")
  expect_error(cp_srs(C = 0.95, n_real = 250, p_est = 1.4, N = 5000), "Parameter 'p_est' must be in the range 0 <= p_est <= 1")
  expect_error(cp_srs(C = 0.95, n_real = 250, p_est = -0.4, N = 5000), "Parameter 'p_est' must be in the range 0 <= p_est <= 1")
  expect_error(cp_srs(C = 0.95, n_real = 250, p_est = 0.4, N = 5000.1), "Parameter 'N' must be a positive integer or Inf")
  expect_error(cp_srs(C = 0.95, n_real = 250, p_est = 0.4, N = -5000), "Parameter 'N' must be a positive integer or Inf")
})

test_that("cp_srs works", {
  result <- cp_srs(C = 0.95, n_real = 250, p_est = 0.4)
  expect_equal(round(result$margin_of_error,4), 0.0608)
  result <- cp_srs(C = 0.95, n_real = 400, p_est = 0.4)
  expect_equal(round(result$margin_of_error,5), 0.04807)
  result <- cp_srs(C = 0.95, n_real = 250, p_est = 0.4, N = 5000)
  expect_equal(round(result$margin_of_error,4), 0.0593)
})

test_that("cp_srs works for example 4.7", {
  result <- cp_srs(C = 0.95, n_real = 100, p_est = 0.15, N = 300)
  expect_equal(round(result$margin_of_error,4), 0.0575)
  result <- cp_srs(C = 0.95, n_real = 100, p_est = 0.65, N = 300)
  expect_equal(round(result$margin_of_error,4), 0.0768)
})

