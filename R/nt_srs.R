#' Sample size needed to estimate the total with a Simple Random Sampling plan
#'
#' @param C Level of confidence (0 <= C <= 1)
#' @param E Sampling error (E > 1).
#' @param sd_exp Expected standard deviation (sd_exp > 0)
#' @param N A positive integer indicating the number of elements in the population.
#'
#' @return The function returns the sample size needed to estimate the total of a variable, consistent with the risk ('C' and 'E') that the auditor is willing to assume.
#' @export
#'
#' @examples nt_srs(C = 0.95, E = 1000, sd_exp = 4.1, N = 1200)
#' @examples nt_srs(C = 0.95, E = 2000, sd_exp = 4.1, N = 1200)
#' @examples nt_srs(C = 0.95, E = 2000, sd_exp = 2.05, N = 1200)


# Sample size function
nt_srs <- function(C, E, sd_exp, N) {

  # Check parameter ranges
  if (C < 0 || C > 1) {
    stop("Parameter 'C' must be in the range 0 <= C <= 1")
  }

  if (E <= 0) {
    stop("Parameter 'E' must be a positive number")
  }

  if (sd_exp < 0) {
    stop("Parameter 'sd_exp' must be a positive number")
  }

  if (N != round(N) || N <= 0) {
    stop("Parameter 'N' must be a positive integer")
  }

  # Formula to obtain the adjusted sample size
  n <- N^2 * qnorm(C + (1 - C) / 2, 0, 1)^2 * sd_exp^2 / E^2
  fcf <- ifelse(is.infinite(N), 1, N / (N + n - 1))
  n_ajusted <- ceiling(n * fcf)
  return(n_ajusted)
}

