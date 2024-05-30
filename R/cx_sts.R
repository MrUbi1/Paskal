#' Confidence interval of the mean under a Stratified Sampling framework
#'
#' @param C Level of confidence (0 <= C <= 1)
#' @param n A vector with the real sample size for each stratum (n > 0)
#' @param x_est A vector with the sample mean of each stratum (0 <= p_est <= 1)
#' @param N A vector with the number of elements in each stratum (N > 0).
#'
#' @return The function returns the interval of confidence of the population mean.
#' @export
#'
#' @examples cx_sts(C = 0.95, n = c(100, 150, 200), x_est = c(3.2, 3.5, 3.7), sd_est = c(0.5, 0.6, 0.4), parameter = TRUE, N = c(200, 250, 300))

# Confidence interval function
cx_sts <- function(C, n, x_est, sd_est, parameter = FALSE, N) {

  # Check parameter ranges
  if (C < 0 || C > 1) {
    stop("Parameter 'C' must be in the range 0 <= C <= 1")
  }

  if (any(n != round(n)) || any(n <= 0)) {
    stop("All elements in 'n' must be positive integers")
  }

  if (any(sd_est < 0)) {
    stop("All elements in 'sd_est' must be positive numbers")
  }

#  if (!all(x_est >= 0 & x_est <= 1)) {
#    stop("All elements in 'p_est' must be in the range 0 <= p_est <= 1")
#  }

  # Ensure 'x_est', 'n', 'sd_est' and 'N' are of the same length
  if (length(x_est) != length(n) ||
      length(x_est) != length(sd_est) ||
      length(x_est) != length(N)) {
    stop("'x_est', 'n', 'sd_est' and 'N' must have the same length")
  }

  # Calculate the standard deviation of the estimated proportions
  sd_x_est <- sqrt(sum(N^2 * (N - n) / N * sd_est^2 / n) / sum(N)^2)

  # Calculate the limit points based on the confidence level
  LP <- ifelse(parameter == TRUE,
               qnorm(C + (1 - C) / 2, 0, 1),
               qt(C + (1 - C) / 2, sum(n))) * sd_x_est

  # Calculate the estimated proportion
  x_est <- sum(N / sum(N) * x_est) # This is the real allocation, so there's no need to use parameter 'alloc'.

  # Calculate the upper and lower bounds of the confidence interval
  p_upper <- round(x_est + LP, 3)
  p_lower <- round(x_est - LP, 3)

  # Generate the inference statement
  inference <- paste0("With ", C * 100, "% confidence, the population mean is between ", p_lower, " and ", p_upper)

  # Output the results
  cat(inference, "\n")
  print(paste('x_est =', x_est))
}
