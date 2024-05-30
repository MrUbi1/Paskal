#' Confidence interval of the total under a Stratified Sampling framework
#'
#' @param C Level of confidence (0 <= C <= 1)
#' @param n A vector with the real sample size for each stratum (n > 0)
#' @param t_est A vector with the sample total of each stratum (0 <= p_est <= 1)
#' @param N A vector with the number of elements in each stratum (N > 0).
#'
#' @return The function returns the interval of confidence of the population total
#' @export
#'
#' @examples ct_sts(C = 0.95, x_est = c(0.3, 0.5, 0.7), n = c(100, 150, 200), N = c(200, 250, 300))

# Confidence interval function
ct_sts <- function(C, n, x_est, sd_est, parameter = FALSE, N) {

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

  # Ensure 't_est', 'n', 'sd_est' and 'N' are of the same length
  if (length(x_est) != length(n) ||
      length(x_est) != length(sd_est) ||
      length(x_est) != length(N)) {
    stop("'x_est', 'n', 'sd_est' and 'N' must have the same length")
  }

  # Calculate the estimated total
  x_est <- sum(N / sum(N) * x_est)
  t_est <- x_est * sum(N)

  # Calculate the standard deviation of the estimated proportions
  sd_t_est <- sqrt(sum(N^2 * (N - n) / N * sd_est^2 / n))

  # Calculate the limit points based on the confidence level
  LP <- ifelse(parameter == TRUE,
               qnorm(C + (1 - C) / 2, 0, 1),
               qt(C + (1 - C) / 2, sum(n))) * sd_t_est

  # Calculate the upper and lower bounds of the confidence interval
  p_lower <- round(t_est - LP, 3)
  p_upper <- round(t_est + LP, 3)

  # Generate the inference statement
  inference <- paste0("With ", C * 100, "% confidence, the population total is between ", p_lower, " and ", p_upper)

  # Output the results
  cat(inference, "\n")
  print(paste('t_est =', t_est))
}
