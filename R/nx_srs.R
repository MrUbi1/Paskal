#' Sample size needed to estimate the mean with a Simple Random Sampling plan
#'
#' @param C Level of confidence (0 <= C <= 1)
#' @param E Sampling error (E > 1).
#' @param sd_exp Expected standard deviation (sd_exp > 0)
#' @param parameter Type TRUE if you do know the populations sd, type FALSE (default) if it is an estimate.
#' @param N A positive integer indicating the number of elements in the population. By default, infinite.
#'
#' @return The function returns the sample size needed to estimate the mean of a phenomena, consistent with the risk ('C' and 'e') that the auditor is willing to assume.
#' @export
#'
#' @examples nx_srs(C = 0.95, E = 50, sd_exp = 400)
#' @examples nx_srs(C = 0.95, E = 50, sd_exp = 400, parameter = TRUE, N = 10000)
#' @examples nx_srs(C = 0.95, E = 50, sd_exp = 400, parameter = FALSE, N = 10000)
#' @examples nx_srs(C = 0.95, E = 100, sd_exp = 400, N = 10000)

# Sample size function
nx_srs <- function(C, E, sd_exp, parameter = FALSE, N = Inf) {

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

  if (!missing(N)) {
    if (!is.infinite(N) && (N != round(N) || N <= 0)) {
      stop("Parameter 'N' must be a positive integer or Inf")
    }
  }

  # Formula to obtain the adjusted sample size
  n <- ifelse(parameter == TRUE,
              qnorm(C + (1 - C) / 2, 0, 1)^2 * sd_exp^2 / E^2,
              qt(C + (1 - C) / 2, N)^2 * sd_exp^2 / E^2
              )

  fcf <- ifelse(is.infinite(N), 1, N / (N + n - 1))

  n_ajusted <- ceiling(n * fcf)

  return(n_ajusted)
}


