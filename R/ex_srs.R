#' Sampling error when estimating the mean with a Simple Random Sampling plan
#'
#' @param C Level of confidence (0 <= C <= 1)
#' @param n_real Given sample size (n_real > 0)
#' @param sd_exp Expected standard deviation (sd_exp > 0)
#' @param N A positive integer indicating the number of elements in the population. By default, infinite.
#'
#' @return The function returns the sample error consistent with the estimation of the mean, given the sample size.
#' @export
#'
#' @examples ex_srs(C = 0.95, n_real = 140, sd_exp = 600)
#' @examples ex_srs(C = 0.95, n_real = 140, sd_exp = 800)
#' @examples ex_srs(C = 0.95, n_real = 140, sd_exp = 800, N = 2000)


# Sample error function
ex_srs <- function(C, n_real, sd_exp, N = Inf) {

  # Check parameter ranges
  if (C < 0 || C > 1) {
    stop("Parameter 'C' must be in the range 0 <= C <= 1")
  }

  if (n_real <= 0 ) {
    stop("Parameter 'n_real' must be a positive integer")
  }

  if (sd_exp < 0) {
    stop("Parameter 'sd_exp' must be a positive number")
  }

  if (!missing(N)) {
    if (!is.infinite(N) && (N != round(N) || N <= 0)) {
      stop("Parameter 'N' must be a positive integer or Inf")
    }
  }

  # Function of difference, aimed to iterate with different values of 'E'
  difference <- function(E) {
    n_theo = (qnorm(C + (1 - C) / 2, 0, 1)^2 * sd_exp^2 / E^2)
    fcf <- ifelse(is.infinite(N), 1, N / (N + n_theo - 1))
    n_adjusted <- n_theo * fcf
    return(abs(n_adjusted - n_real))
  }

  # Find the value of 'E' that minimizes the difference
  result <- optimize(f = difference, interval = c(0, 1000000))

  # Return the result containing the optimal (minimum) 'E' value
  return(result$minimum)
}


