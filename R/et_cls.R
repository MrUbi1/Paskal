#' Sampling error when estimating the total using a cluster sampling design
#'
#' @param C Level of confidence; 0 <= C <= 1.
#' @param n_real A positive integer representing the real sample size (the number of clusters sampled); n_real > 0.
#' @param sd_est Estimated standard deviation ; sd_est(i) > 0.
#' @param N A positive integer representing the size of the population (the total number of clusters); N(i) > 0.
#' @param parameter Type TRUE if you do know the populations SD in sd_exp, or type FALSE (default) if they are estimates.
#'
#' @return This function returns the sampling error when using a cluster sampling design without replacement to estimate the total, given the sample size.
#' @export
#'
#' @examples et_cls(C = 0.95, n_real = 213, sd_est = 25189, N = 415, parameter = TRUE)
#' # Reversa de nt_cls(C = 0.95, E = 1000000, sd_exp = 25189, N = 415, parameter = TRUE) = 213

# Sample error function
et_cls <- function(C, n_real, sd_est, N = Inf, parameter = FALSE) {

  # Check parameter ranges
  if (C < 0 || C > 1) {
    stop("Parameter 'C' must be in the range 0 <= C <= 1")
  }

  if (n_real < 0) {
    stop("Parameter 'n_real' must be greater than 0")
  }

  if (sd_est < 0) {
    stop("Parameter 'sd_est' must be a positive number")
  }

  if (!missing(N)) {
    if (!is.infinite(N) && (N != round(N) || N <= 0)) {
      stop("Parameter 'N' must be a positive integer or Inf")
    }
  }


  # Function of difference, aimed to iterate with different values of 'E' (Ref. 5.6)
  difference <- function(E) {
    N <- ifelse(is.infinite(N), 9999999, N) # CHEQUEAR ESTO

    n_adjusted <- if (parameter) {
      Z <- qnorm(C + (1 - C) / 2, 0, 1) # qnorm: quantile of the normal distribution
      (N * sd_est^2) / ((N * E^2 / (Z^2 * N^2)) + sd_est^2)

    } else {
      t <- qt(C + (1 - C) / 2, sum(n_real) - 1) # qt: quantile of the t-student distribution
      (N * sd_est^2) / ((N * E^2 / (t^2 * N^2)) + sd_est^2)
    }

    return(abs(sum(n_adjusted - n_real)))
  }

  # Find the value of 'E' that minimizes the difference
  result <- optimize(f = difference, interval = c(0.001, 1000000))

  return(list(E = result$minimum))
}


