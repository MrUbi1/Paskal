#' Sample size 'n' needed to estimate the proportion 'p' with a Simple Random Sampling (SRS) plan, given the sampling error 'e'
#'
#' @param C Level of confidence. (0 <= C <= 1)
#' @param p Expected proportion in the population. By default 0.5, also applicable if you lack information. (0 <= p <= 1)
#' @param e Sampling error. (0 <= e <= 1).
#' @param N Population size, by default is infinite. Must be a positive integer.
#'
#' @return The function returns the sample size needed to estimate the proportion of occurrence of a phenomena, consistent with the risk ('C' and 'e') that the auditor is willing to assume, when conducting a Simple Random Sampling plan, among others with some restrictions.
#' @export
#'
#' @examples np_srs(0.90, 0.04)
#' @examples np_srs(0.90, 0.04, 0.5)
#' @examples np_srs(0.90, 0.04, 0.5, 20000)
#' @examples np_srs(0.90, 0.04, N = 20000)

# Sample size function
np_srs <- function(C, e, p = 0.5, N = Inf) {

  # Check parameter ranges
  if (C < 0 || C > 1) {
    stop("Parameter 'C' must be in the range 0 <= C <= 1")
  }

  if (e < 0 || e > 1) {
    stop("Parameter 'e' must be in the range 0 <= e <= 1")
  }

  if (p < 0 || p > 1) {
    stop("Parameter 'p' must be in the range 0 <= p <= 1")
  }

  if (!missing(N)) {
    if (!is.infinite(N) && (N != round(N) || N <= 0)) {
      stop("Parameter 'N' must be a positive integer or Inf")
    }
  }

  # Formula to obtain the adjusted sample size
  n <- qnorm(C + (1 - C) / 2, 0, 1)^2 * p * (1 - p) / e^2
  fcf <- ifelse(is.infinite(N), 1, N / (N + n - 1))
  n_adjusted <- ceiling(n * fcf)
  return(n_adjusted)
}



