
test_that("Parameter ranges works ok", {
  expect_error(ep_srs(C = 1.90, n_real = 415, p_est = 0.4, N = 10000), "Parameter 'C' must be in the range 0 <= C <= 1")
  expect_error(ep_srs(C = -0.90, n_real = 415, p_est = 0.4, N = 10000), "Parameter 'C' must be in the range 0 <= C <= 1")
  expect_error(ep_srs(C = 0.90, n_real = 415.1, p_est = 0.4, N = 10000), "Parameter 'n_real' must be a positive integer")
  expect_error(ep_srs(C = 0.90, n_real = -415, p_est = 0.4, N = 10000), "Parameter 'n_real' must be a positive integer")
  expect_error(ep_srs(C = 0.90, n_real = 415, p_est = 1.4, N = 10000), "Parameter 'p_est' must be in the range 0 <= p_est <= 1")
  expect_error(ep_srs(C = 0.90, n_real = 415, p_est = -0.4, N = 10000), "Parameter 'p_est' must be in the range 0 <= p_est <= 1")
  expect_error(ep_srs(C = 0.90, n_real = 415, p_est = 0.4, N = 10000.1), "Parameter 'N' must be a positive integer or Inf")
  expect_error(ep_srs(C = 0.90, n_real = 415, p_est = 0.4, N = -10000), "Parameter 'N' must be a positive integer or Inf")
})

test_that("ep_srs works", {
  result <- ep_srs(C = 0.90, n_real = 415)
  expect_equal(round(result$e, 4), 0.0404)
  result <- ep_srs(C = 0.90, n_real = 415, p_est = 0.4)
  expect_equal(round(result$e, 4), 0.0396)
  result <- ep_srs(C = 0.90, n_real = 415, p_est = 0.4, N = 10000)
  expect_equal(round(result$e, 4), 0.0387)
  result <- ep_srs(C = 0.90, n_real = 415, N = 10000)
  expect_equal(round(result$e, 4), 0.0395)
})

test_that("ep_srs works for reverse of example 4.8", {
  result <- ep_srs(C = 0.95, n_real = 323, p_est = 0.5, N = 2000)
  expect_equal(round(result$e, 2), 0.05)
})
