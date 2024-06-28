
test_that("Parameter ranges works ok", {
  expect_error(nx_srs(C = 1.1, E = 50, sd_exp = 400), "Parameter 'C' must be in the range 0 <= C <= 1")
  expect_error(nx_srs(C = -0.1, E = 50, sd_exp = 400), "Parameter 'C' must be in the range 0 <= C <= 1")
  expect_error(nx_srs(C = 0.95, E = -50, sd_exp = 400), "Parameter 'E' must be a positive number")
  expect_error(nx_srs(C = 0.95, E = 50, sd_exp = -1), "Parameter 'sd_exp' must be a positive number")
  expect_error(nx_srs(C = 0.95, E = 50, sd_exp = 10, N = 1.1), "Parameter 'N' must be a positive integer or Inf")
  expect_error(nx_srs(C = 0.95, E = 50, sd_exp = 10, N = 0), "Parameter 'N' must be a positive integer or Inf")
})

test_that("nt_srs works", {
  result <- nt_srs(C = 0.95, E = 1000, sd_exp = 4.1, N = 1200)
  expect_equal(result$n, 87)
  result <- nt_srs(C = 0.95, E = 1500, sd_exp = 4.1, N = 1200)
  expect_equal(result$n, 41)
  result <- nt_srs(C = 0.95, E = 2000, sd_exp = 3, N = 5000, parameter = TRUE)
  expect_equal(result$n, 208)
})


