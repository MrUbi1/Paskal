#' Confidence interval of the proportion under a Simple Random Sampling framework
#'
#' @param C Level of confidence (0 <= C <= 1)
#' @param n A vector with the real sample size for each stratum (n > 0)
#' @param p_est A vector with the sample proportion of each stratum (0 <= p_est <= 1)
#' @param N A vector indicating the number of elements in each stratum (N > 0).
#'
#' @return The function returns the interval of confidence of the population proportion.
#' @export
#'
#' @examples cp_sts(C = 0.95, p_est = c(0.3, 0.5, 0.7), n = c(100, 150, 200), N = c(200, 250, 300))

# Confidence interval function
cp_sts <- function(C, n, p_est, N) {

  # Check parameter ranges
  if (C < 0 || C > 1) {
    stop("Parameter 'C' must be in the range 0 <= C <= 1")
  }

  if (any(n != round(n)) || any(n <= 0)) {
    stop("All elements in 'n' must be positive integers")
  }

  if (!all(p_est >= 0 & p_est <= 1)) {
    stop("All elements in 'p_est' must be in the range 0 <= p_est <= 1")
  }

  # Ensure 'p_est', 'n', and 'N' are of the same length
  if (length(p_est) != length(n) || length(p_est) != length(N)) {
    stop("'p_est', 'n', and 'N' must have the same length")
  }

  # Calculate the standard deviation of the estimated proportions
  sd_p_est <- sqrt(sum(N^2 * (N - n) / N * p_est * (1 - p_est) / (n - 1)) / sum(N)^2)

  # Calculate the limit points based on the confidence level
  LP <- qnorm(C + (1 - C) / 2, 0, 1) * sd_p_est # ojo no hay parameter para la 't'

  # Calculate the estimated proportion
  p_est <- sum(N / sum(N) * p_est)

  # Calculate the upper and lower bounds of the confidence interval
  p_upper <- round(min(1, p_est + LP), 3)
  p_lower <- round(max(0, p_est - LP), 3)

  # Generate the inference statement
  inference <- paste0("With ", C * 100, "% confidence, the proportion in the population is between ", p_lower, " and ", p_upper)

  # Output the results
  cat(inference, "\n")
  print(paste('p_est =', p_est))
}
