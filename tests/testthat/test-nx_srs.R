
test_that("Parameter ranges works ok", {
  expect_error(nx_srs(C = 1.1, E = 50, sd_exp = 400), "Parameter 'C' must be in the range 0 <= C <= 1")
  expect_error(nx_srs(C = -0.1, E = 50, sd_exp = 400), "Parameter 'C' must be in the range 0 <= C <= 1")
  expect_error(nx_srs(C = 0.95, E = -50, sd_exp = 400), "Parameter 'E' must be a positive number")
  expect_error(nx_srs(C = 0.95, E = 50, sd_exp = -1), "Parameter 'sd_exp' must be a positive number")
  expect_error(nx_srs(C = 0.95, E = 50, sd_exp = 10, N = 1.1), "Parameter 'N' must be a positive integer or Inf")
  expect_error(nx_srs(C = 0.95, E = 50, sd_exp = 10, N = 0), "Parameter 'N' must be a positive integer or Inf")
})

test_that("nx_srs works", {
  result <- nx_srs(C = 0.95, E = 50, sd_exp = 400)
  expect_equal(result$n, 246)
  result <- nx_srs(C = 0.95, E = 50, sd_exp = 400, parameter = TRUE, N = 10000)
  expect_equal(result$n, 240)
})



