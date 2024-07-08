#' Sampling error when estimating the proportion using a stratified sampling design
#'
#' @param C Level of confidence; 0 <= C <= 1.
#' @param n_real A vector of positive integers representing the real sample size of each stratum; n_real(i) > 0.
#' @param p_est A vector with the estimated proportion in each stratum; 0 <= p_est(i) <= 1, for every 'i' stratum.
#' @param alloc A vector with the relative allocation of sample size for each stratum; 0 < alloc(i) < 1, where sum(alloc(i)) = 1. If not defined (default), its values would be proportional to the size of each stratum.
#' @param N A vector of positive integers representing the number of elements in each stratum; N(i) > 0.
#'
#' @return This function returns the global sampling error when using a stratified sampling design without replacement to estimate the proportion, given the sample size.
#' @export
#'
#' @details
#' The function looks for the value of 'e' that, given the real sampling size for each stratum, fits:
#' \deqn{n = \frac{\sum_{i=1}^{s} \frac{N_i^2 \cdot \text{p}_i \cdot (1 - \text{p}_i)}{\text{alloc}_i}}{N^2 \cdot \frac{e^2}{Z^2} + \sum_{i=1}^{s} N_i \cdot \text{p}_i \cdot (1 - \text{p}_i) }}
#' where p is parameter 'p_est', and 'Z' is the quantile of the two-tailed normal distribution function,
#' compatible with the chosen confidence level 'C'.
#'
#' @examples ep_sts(C = 0.95, n_real = c(66, 9, 9), p_est = c(0.2, 0.5, 0.7), alloc = c(0.8, 0.1, 0.1), N = c(1400, 400, 200))



# Sample error function
ep_sts <- function(C, n_real, p_est, alloc = NULL, N) {

  # Default allocation if 'alloc' is not provided
  if (is.null(alloc)) {
    alloc <- N / sum(N)
  }

  # Check parameter ranges
  if (C < 0 || C > 1) {
    stop("Parameter 'C' must be in the range 0 <= C <= 1")
  }

  if (any(n_real != round(n_real)) || any(n_real <= 0)) {
    stop("All elements in 'n_real' must be positive integers")
  }

  if (any(p_est < 0 | p_est > 1)) {
    stop("All elements in 'p_est' must be in the range 0 <= p_est <= 1")
  }

  if (any(alloc <= 0 | alloc > 1)) {
    stop("All elements in 'alloc' must be in the range 0 < alloc <= 1")
  }

  if (abs(sum(alloc) - 1) > .Machine$double.eps^0.5) {
    stop("The sum of elements in 'alloc' must be equal to 1")
  }

  if (any(N != round(N)) || any(N <= 0)) {
    stop("All elements in 'N' must be positive integers")
  }

  # Ensure 'n_real', 'p_est', 'alloc', and 'N' are of the same length
  if (length(p_est) != length(alloc) ||
      length(p_est) != length(N) ||
      length(p_est) != length(n_real)) {
    stop("'n_real', 'p_est', 'alloc', and 'N' must have the same length")
  }

  # Function of difference, aimed to iterate with different values of 'e'
  difference <- function(e) {
    n_adjusted <- {
      Z <- qnorm(C + (1 - C) / 2, 0, 1) # qnorm: quantile of the normal distribution
      sum(N^2 * p_est * (1 - p_est) / alloc) /
        (sum(N)^2 * e^2 / Z^2 + sum(N * p_est * (1 - p_est)))
    }

    return(abs(sum(n_adjusted * alloc - n_real)))
  }

  # Find the value of 'e' that minimizes the difference
  result <- optimize(f = difference, interval = c(0.001, 1))

  # Return the result containing the optimal (minimum) 'e' value
  return(list(e = result$minimum))
}

