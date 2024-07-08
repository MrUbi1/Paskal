#' Sample size required to estimate the total using a cluster sampling design
#'
#' @param C Level of confidence; 0 <= C <= 1.
#' @param E Sampling error; E > 0.
#' @param sd_exp Expected standard deviation; sd_exp > 0.
#' @param N A positive integer indicating the population size (id est, the total number of clusters). Defaults to infinite.
#' @param parameter Type TRUE if you do know the populations sd, type FALSE (default) if it is an estimate.
#'
#' @return This function returns the sample size required to estimate the total of a variable when using a cluster sampling design without replacement, given the level of risk.
#' @export
#'
#' @details
#' The function to calculate the sample size is:
#' \deqn{n = \frac{N \cdot \text{sd}^2}{\frac{E^2}{N \cdot Z^2} + \text{sd}^2}}
#' where 'sd' is parameter 'sd_exp', and 'Z' is the quantile of the two-tailed normal distribution function, compatible with the chosen confidence level 'C'.
#' If 'sd_exp' is unknown, the t-student is used instead of the normal distribution.
#'
#' @examples nt_cls(C = 0.95, E = 500000, sd_exp = 5000, N = 400)


# Sample size function
nt_cls <- function(C, E, sd_exp, N = Inf, parameter = FALSE) {

  # Check parameter ranges
  if (C < 0 || C > 1) {
    stop("Parameter 'C' must be in the range 0 <= C <= 1")
  }

  if (E <= 0) {
    stop("Parameter 'E' must be a positive number")
  }

  if (sd_exp <= 0) {
    stop("Parameter 'sd_exp' must be a positive number")
  }

  if (!missing(N)) {
    if (!is.infinite(N) && (N != round(N) || N <= 0)) {
      stop("Parameter 'N' must be a positive integer or Inf")
    }
  }

  # Formula to obtain the adjusted sample size
  N <- ifelse(is.infinite(N), 10^10, N)

  n <- if (parameter) {
    Z <- qnorm(C + (1 - C) / 2, 0, 1) # qnorm: quantile of the normal distribution
    (N * sd_exp^2) / ((N * E^2 / (Z^2 * N^2)) + sd_exp^2)

  } else {
    t <- qt(C + (1 - C) / 2, sum(N) - 1) # qt: quantile of the t-student distribution
    (N * sd_exp^2) / ((N * E^2 / (t^2 * N^2)) + sd_exp^2)
  }

  return(list(n = ceiling(n)))

}


