#' Sample size required to estimate the proportion using a stratified sampling design
#'
#' @param C Level of confidence; 0 <= C <= 1.
#' @param e Sampling error; 0 <= e <= 1.
#' @param p_exp A vector with the expected proportion in each stratum; 0 <= p_exp(i) <= 1, for every 'i' stratum.
#' @param alloc A vector with the relative allocation of sample size for each stratum; 0 < alloc(i) < 1, where sum(alloc(i)) = 1. If not defined (default), its values would be proportional to the size of each stratum.
#' @param N A vector of positive integers representing the number of elements in each stratum.
#'
#' @return This function returns the sample size required to estimate the proportion of occurrences of an event when using a stratified sampling design without replacement, given the level of risk.
#' @export
#'
#' @details
#' The function to calculate the sample size is:
#' \deqn{n = \frac{\sum_{i=1}^{s} \frac{N_i^2 \cdot \text{p}_i \cdot (1 - \text{p}_i)}{\text{alloc}_i}}{N^2 \cdot \frac{e^2}{Z^2} + \sum_{i=1}^{s} N_i \cdot \text{p}_i \cdot (1 - \text{p}_i) }}
#' where p is parameter 'p_exp', and 'Z' is the quantile of the two-tailed normal distribution function,
#' compatible with the chosen confidence level 'C'.
#'
#' @examples np_sts(C = 0.95, e = 0.1, p_exp = c(0.2, 0.3), N = c(220, 350))
#' @examples np_sts(C = 0.95, e = 0.1, p_exp = c(0.1, 0.5, 0.5), alloc = c(0.3, 0.3, 0.4), N = c(150, 40, 110))


# Sample size function
np_sts <- function(C, e, p_exp, alloc = NULL, N) {

  # Default allocation if 'alloc' is not provided
  if (is.null(alloc)) {
    alloc <- N / sum(N)
  }

  # Check parameter ranges
  if (C < 0 || C > 1) {
    stop("Parameter 'C' must be in the range 0 <= C <= 1")
  }

  if (e < 0 || e > 1) {
    stop("Parameter 'e' must be in the range 0 <= e <= 1")
  }

  if (!all(p_exp >= 0 & p_exp <= 1)) {
    stop("All elements in 'p_exp' must be in the range 0 <= p_exp <= 1")
  }

  if (!all(alloc >= 0 & alloc <= 1)) {
    stop("All elements in 'alloc' must be in the range 0 <= alloc <= 1")
  }

  if (abs(sum(alloc) - 1) > .Machine$double.eps^0.5) {
    stop("The sum of elements in 'alloc' must be equal to 1")
  }

  if (!all(N == round(N) & N > 0)) {
    stop("All elements in 'N' must be positive integers")
  }

  if (length(p_exp) != length(alloc) || length(p_exp) != length(N)) {
    stop("'p_exp', 'alloc', and 'N' must have the same length")
  }

  # Formula to obtain the adjusted sample size
  Z <- qnorm(C + (1 - C) / 2, 0, 1) # qnorm: quantile of the normal distribution
  n = sum(N^2 * p_exp * (1 - p_exp) / alloc) / (sum(N)^2 * e^2 / Z^2 + sum(N * p_exp * (1 - p_exp)))

  n_adjusted_i <- ceiling(n * alloc)

  return(list(n = sum(n_adjusted_i), n_i = n_adjusted_i))

}



