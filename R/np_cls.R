#' Sample size required to estimate the proportion using a cluster sampling design
#'
#' @param C Level of confidence; 0 <= C <= 1.
#' @param e Sampling error; 0 <= e <= 1.
#' @param sd_exp Expected standard deviation; sd_exp > 0.
#' @param m Average cluster size (number of elements), either of the population -preferred- or of a preliminary sample; m > 0.
#' @param N A positive integer indicating the population size (id est, the total number of clusters). Defaults to infinite.
#' @param parameter Type TRUE if you do know the populations sd, type FALSE (default) if it is an estimate.
#'
#' @return This function returns the sample size required to estimate the proportion of a variable when using a cluster sampling design without replacement, given the level of risk.
#' @export
#'
#' @examples np_cls(C = 0.95, e = 0.04, sd_exp = 0.726, m = 8, N = 500)


# Sample size function
np_cls <- function(C, e, sd_exp, m, N = Inf, parameter = FALSE) {

  # Check parameter ranges
  if (C < 0 || C > 1) {
    stop("Parameter 'C' must be in the range 0 <= C <= 1")
  }

  if (e < 0 || e > 1) {
    stop("Parameter 'e' must be in the range 0 <= e <= 1")
  }

  if (sd_exp <= 0) {
    stop("Parameter 'sd_exp' must be a positive number")
  }

  if (m <= 0) {
    stop("Parameter 'm' must be a positive number")
  }

  if (!missing(N)) {
    if (!is.infinite(N) && (N != round(N) || N <= 0)) {
      stop("Parameter 'N' must be a positive integer or Inf")
    }
  }

  # Formula to obtain the adjusted sample size (Ref. 5.6)
  N <- ifelse(is.infinite(N), 10^10, N)

  n <- if (parameter) {
    Z <- qnorm(C + (1 - C) / 2, 0, 1) # qnorm: quantile of the normal distribution
    (N * sd_exp^2) / ((N * e^2 * m^2 / Z^2) + sd_exp^2)

  } else {
    t <- qt(C + (1 - C) / 2, sum(N) - 1) # qt: quantile of the t-student distribution
    (N * sd_exp^2) / ((N * e^2 * m^2 / t^2) + sd_exp^2)
  }

  return(list(n = ceiling(n)))

}

#Dudas
# Con la t-student no me queda claro si df = 'N', o 'n', o 'n-1'. Si fuera n, como hago?
