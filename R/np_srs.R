#' Sample size needed to estimate the proportion with a Simple Random Sampling plan
#'
#' @param C Level of confidence. (0 <= C <= 1)
#' @param p_exp Expected proportion in the population. By default 0.5, also applicable if you lack information. (0 <= p_exp <= 1)
#' @param e Sampling error. (0 <= e <= 1).
#' @param N A positive integer indicating the number of elements in the population. By default, infinite.
#'
#' @return The function returns the sample size needed to estimate the proportion of occurrence of a phenomena, consistent with the risk ('C' and 'e') that the auditor is willing to assume, when conducting a Simple Random Sampling plan, among others with some restrictions.
#' @export
#'
#' @examples np_srs(C = 0.90, e = 0.04)
#' @examples np_srs(C = 0.90, e = 0.04, p_exp = 0.5)
#' @examples np_srs(C = 0.90, e = 0.04, p_exp = 0.5, N = 20000)
#' @examples np_srs(C = 0.90, e = 0.04, N = 20000)

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

  # Formula to obtain the adjusted sample size
  n <- qnorm(C + (1 - C) / 2, 0, 1)^2 * p_exp * (1 - p_exp) / e^2
  fcf <- ifelse(is.infinite(N), 1, N / (N + n - 1))
  n_adjusted <- ceiling(n * fcf)
  return(n_adjusted)
}



