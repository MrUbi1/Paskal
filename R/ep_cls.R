#' Sampling error when estimating the proportion using a cluster sampling design
#'
#' @param C Level of confidence; 0 <= C <= 1.
#' @param n_real A positive integer representing the real sample size (the number of clusters sampled); n_real > 0.
#' @param sd_est Estimated standard deviation ; sd_est > 0.
#' @param m Average cluster size (number of elements), either of the population -preferred- or of a preliminary sample; m > 0.
#' @param N A positive integer indicating the population size (id est, the total number of clusters). Defaults to infinite.
#' @param parameter Type TRUE if you do know the populations SD in sd_exp, or type FALSE (default) if they are estimates.
#'
#' @return This function returns the sampling error when using a cluster sampling design without replacement to estimate the proportion, given the sample size.
#' @export
#'
#' @details
#' The function looks for the value of 'e' that, given the real sampling size, fits:
#' \deqn{n = \frac{N \cdot \text{sd}^2}{\frac{N \cdot e^2 \cdot m^2}{Z^2} + \text{sd}^2}}
#' where 'sd' is parameter 'sd_est', and 'Z' is the quantile of the two-tailed normal distribution function,
#' compatible with the chosen confidence level 'C'.
#'
#' @examples ep_cls(C = 0.95, n_real = 31, sd_est = 0.726, m = 6.04, N = 415, parameter = TRUE)


# Sample error function
ep_cls <- function(C, n_real, sd_est, m, N = Inf, parameter = FALSE) {

  # Check parameter ranges
  if (C < 0 || C > 1) {
    stop("Parameter 'C' must be in the range 0 <= C <= 1")
  }

  if (n_real != round(n_real) || n_real <= 0) {
    stop("Parameter 'n_real' must be a positive integer")
  }

  if (sd_est <= 0) {
    stop("Parameter 'sd_est' must be a positive number")
  }

  if (m <= 0) {
    stop("Parameter 'm' must be a positive number")
  }

  if (!missing(N)) {
    if (!is.infinite(N) && (N != round(N) || N <= 0)) {
      stop("Parameter 'N' must be a positive integer or Inf")
    }
  }


  # Function of difference, aimed to iterate with different values of 'e'
  difference <- function(e) {
    N <- ifelse(is.infinite(N), 10^10, N)

    n_adjusted <- if (parameter) {
      Z <- qnorm(C + (1 - C) / 2, 0, 1) # qnorm: quantile of the normal distribution
      (N * sd_est^2) / ((N * e^2 * m^2 / Z^2) + sd_est^2)

    } else {
      t <- qt(C + (1 - C) / 2, sum(n_real) - 1) # qt: quantile of the t-student distribution
      (N * sd_est^2) / ((N * e^2 * m^2 / t^2) + sd_est^2)
    }

    return(abs(sum(n_adjusted - n_real)))
  }

  # Find the value of 'E' that minimizes the difference
  result <- optimize(f = difference, interval = c(0.001, 1))

  return(list(e = result$minimum))
}


