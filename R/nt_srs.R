#' Sample size required to estimate the total using a simple random sampling design
#'
#' @param C Level of confidence; 0 <= C <= 1.
#' @param E Sampling error; E > 0.
#' @param sd_exp Expected standard deviation; sd_exp > 0.
#' @param N A positive integer representing the number of elements in the population.
#' @param parameter Type TRUE if you do know the population SD, or type FALSE (default) if it is an estimate.
#'
#' @return This function returns the sample size required to estimate the total of a variable when using a simple random sampling design without replacement, given the level of risk.
#' @export
#'
#' @examples nt_srs(C = 0.95, E = 1000, sd_exp = 4.1, N = 1200)
#' @examples nt_srs(C = 0.95, E = 1500, sd_exp = 4.1, N = 1200)
#' @examples nt_srs(C = 0.95, E = 2000, sd_exp = 3, N = 5000, parameter = TRUE)


# Sample size function
nt_srs <- function(C, E, sd_exp, N, parameter = FALSE) {

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
  n <- ifelse(parameter == TRUE,
              N^2 * qnorm(C + (1 - C) / 2, 0, 1)^2 * sd_exp^2 / E^2, # qnorm: quantile of the normal distribution
              N^2 * qt(C + (1 - C) / 2, N)^2 * sd_exp^2 / E^2 # qt: quantile of the t-student distribution
              )

  fcf <- ifelse(is.infinite(N), 1, N / (N + n - 1))

  n_adjusted <- ceiling(n * fcf)

  return(list(n = n_adjusted))
}

#Dudas
# No me gusta de la fórmula que un N = Inf tiende a n = Inf. Es raro. Chequeé en ambos libros y las fórmulas son equivalentes.
