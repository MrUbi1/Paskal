#' Sample size required to estimate the proportion using a simple random sampling design
#'
#' @param C Level of confidence; 0 <= C <= 1.
#' @param e Sampling error; 0 <= e <= 1.
#' @param p_exp Expected proportion in the population; 0 <= p_exp <= 1. Enter 0.5 (default) if you lack information.
#' @param N A positive integer representing the number of elements in the population. Defaults to infinite.
#'
#' @return This function returns the sample size required to estimate the proportion of occurrences of an event when using a simple random sampling design without replacement, given the level of risk.
#' @export
#'
#' @examples np_srs(C = 0.90, e = 0.04)
#' @examples np_srs(C = 0.90, e = 0.1, p_exp = 0.5)
#' @examples np_srs(C = 0.90, e = 0.1, p_exp = 0.5, N = 1000)
#' @examples np_srs(C = 0.90, e = 0.1, p_exp = 0.2, N = 1000)

# Sample size function
np_srs <- function(C, e, p_exp = 0.5, N = Inf) {

  # Check parameter ranges
  if (C < 0 || C > 1) {
    stop("Parameter 'C' must be in the range 0 <= C <= 1")
  }

  if (e < 0 || e > 1) {
    stop("Parameter 'e' must be in the range 0 <= e <= 1")
  }

  if (p_exp < 0 || p_exp > 1) {
    stop("Parameter 'p_exp' must be in the range 0 <= p_exp <= 1")
  }

  if (!missing(N)) {
    if (!is.infinite(N) && (N != round(N) || N <= 0)) {
      stop("Parameter 'N' must be a positive integer or Inf")
    }
  }

  # Formula for determining the adjusted sample size
  n <- qnorm(C + (1 - C) / 2, 0, 1)^2 * p_exp * (1 - p_exp) / e^2 # qnorm: quantile of the normal distribution

  fcf <- ifelse(is.infinite(N), 1, N / (N + n - 1))

  n_adjusted <- ceiling(n * fcf)

  return(list(n = n_adjusted))
}
