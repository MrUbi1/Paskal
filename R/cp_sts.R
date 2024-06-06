#' Confidence interval of the proportion under a stratified sampling design
#'
#' @param C Level of confidence; 0 <= C <= 1.
#' @param n_real A vector of positive integers representing the real sample size of each stratum; n_real(i) > 0, for every 'i' stratum.
#' @param p_est A vector with the estimated proportion in each stratum; 0 <= p_exp(i) <= 1.
#' @param N A vector of positive integers representing the number of elements in each stratum; N(i) > 0.
#'
#' @return This function returns the global confidence interval when using a stratified sampling design without replacement to estimate the proportion, given the sample size.
#' @export
#'
#' @examples cp_sts(C = 0.95, n_real = c(100, 150, 200), p_est = c(0.3, 0.5, 0.7), N = c(200, 250, 300))



# Confidence interval function
cp_sts <- function(C, n_real, p_est, N) {

  # Check parameter ranges
  if (C < 0 || C > 1) {
    stop("Parameter 'C' must be in the range 0 <= C <= 1")
  }

  if (any(n_real != round(n_real)) || any(n_real <= 0)) {
    stop("All elements in 'n_real' must be positive integers")
  }

  if (!all(p_est >= 0 & p_est <= 1)) {
    stop("All elements in 'p_est' must be in the range 0 <= p_est <= 1")
  }

  if (any(N != round(N)) || any(N <= 0)) {
    stop("All elements in 'N' must be positive integers")
  }

  # Ensure 'p_est', 'n_real', and 'N' are of the same length
  if (length(p_est) != length(n_real) || length(p_est) != length(N)) {
    stop("'p_est', 'n_real', and 'N' must have the same length")
  }

  # Calculate the confidence interval (Ref. 5.14)

  sd_p_est <- sqrt(sum(N^2 * (N - n_real) / N * p_est * (1 - p_est) / (n_real - 1)) / sum(N)^2)

  LP <- qnorm(C + (1 - C) / 2, 0, 1) * sd_p_est # qnorm: quantile of the normal distribution

  p_est <- sum(N / sum(N) * p_est)

  p_upper <- round(min(1, p_est + LP), 3)

  p_lower <- round(max(0, p_est - LP), 3)

  inference <- paste0("The population proportion is between ", p_lower, " and ", p_upper, " with ", C * 100, "% confidence.")

  return(list(global_p_est = p_est, margin_of_error = LP, inference = inference))

}
