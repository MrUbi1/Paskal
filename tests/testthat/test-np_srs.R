# Check parameter ranges

test_that("Parameter ranges works ok", {
  expect_error(np_srs(C = 1.1, e = 0.1, p_exp = 0.2, N = 1000), "Parameter 'C' must be in the range 0 <= C <= 1")
  expect_error(np_srs(C = -0.1, e = 0.1, p_exp = 0.2, N = 1000), "Parameter 'C' must be in the range 0 <= C <= 1")
  expect_error(np_srs(C = 0.90, e = 1.1, p_exp = 0.2, N = 1000), "Parameter 'e' must be in the range 0 <= e <= 1")
  expect_error(np_srs(C = 0.90, e = -0.1, p_exp = 0.2, N = 1000), "Parameter 'e' must be in the range 0 <= e <= 1")
  expect_error(np_srs(C = 0.90, e = 0.1, p_exp = 1.1, N = 1000), "Parameter 'p_exp' must be in the range 0 <= p_exp <= 1")
  expect_error(np_srs(C = 0.90, e = 0.1, p_exp = -0.1, N = 1000), "Parameter 'p_exp' must be in the range 0 <= p_exp <= 1")
  expect_error(np_srs(C = 0.90, e = 0.1, p_exp = 0.2, N = 1.1), "Parameter 'N' must be a positive integer or Inf")
  expect_error(np_srs(C = 0.90, e = 0.1, p_exp = 0.2, N = 0), "Parameter 'N' must be a positive integer or Inf")
})

# Check function

test_that("np_srs works", {
  result <- np_srs(C = 0.90, e = 0.1, p_exp = 0.2, N = 1000)
  expect_equal(result$n, 42)
})


