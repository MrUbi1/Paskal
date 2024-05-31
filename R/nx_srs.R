#' Sample size required to estimate the mean using a simple random sampling design
#'
#' @param C Level of confidence; 0 <= C <= 1.
#' @param E Sampling error; E > 0.
#' @param sd_exp Expected standard deviation; sd_exp > 0.
#' @param N A positive integer representing the number of elements in the population. Defaults to infinite.
#' @param parameter Type TRUE if you do know the population SD, or type FALSE (default) if it is an estimate.
#'
#' @return This function returns the sample size required to estimate the mean of a variable when using a simple random sampling design without replacement, given the level of risk.
#' @export
#'
#' @examples nx_srs(C = 0.95, E = 50, sd_exp = 400)
#' @examples nx_srs(C = 0.95, E = 50, sd_exp = 400, parameter = TRUE, N = 10000)
#' @examples nx_srs(C = 0.95, E = 50, sd_exp = 400, N = 10000, parameter = FALSE)
#' @examples nx_srs(C = 0.95, E = 100, sd_exp = 400, N = 10000)

# Sample size function
nx_srs <- function(C, E, sd_exp, N = Inf, parameter = FALSE) {

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
              qnorm(C + (1 - C) / 2, 0, 1)^2 * sd_exp^2 / E^2, # qnorm: quantile of the normal distribution
              qt(C + (1 - C) / 2, N)^2 * sd_exp^2 / E^2 # qt: quantile of the t-student distribution
              )

  fcf <- ifelse(is.infinite(N), 1, N / (N + n - 1))

  n_adjusted <- ceiling(n * fcf)

  return(list(n = n_adjusted))
}

#Dudas
# Con la t-student no me queda claro si df = 'N', o 'n', o 'n-1'. Si fuera n, como hago?
