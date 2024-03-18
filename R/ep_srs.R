#' Sampling error when estimating the proportion with a Simple Random Sampling plan
#'
#' @param C Level of confidence (0 <= C <= 1)
#' @param n Given sample size
#' @param p Expected or estimated (preferred) proportion in the population.
#' @param N Population size. Must be a positive integer.
#'
#' @return The function returns the sample error consistent with the estimation of the proportion of occurrence of a phenomena, given the sample size.
#' @export
#'
#' @examples ep_srs(0.90, 415)
#' @examples ep_srs(0.90, 415, 0.5)
#' @examples ep_srs(0.90, 415, 0.5, 20000)
#' @examples ep_srs(0.90, 415, N = 20000)

# Sample error function
ep_srs <- function(C, n, p = 0.5, N = Inf) {

  # Check parameter ranges
  if (C < 0 || C > 1) {
    stop("Parameter 'C' must be in the range 0 <= C <= 1")
  }

  if (n <= 0 ) {
    stop("Parameter 'n' must be higher than 0")
  }

  if (p < 0 || p > 1) {
    stop("Parameter 'p' must be in the range 0 <= p <= 1")
  }

  if (!missing(N)) {
    if (!is.infinite(N) && (N != round(N) || N <= 0)) {
      stop("Parameter 'N' must be a positive integer or Inf")
    }
  }

  # Function of difference, aimed to iterate with different values of 'e'
  difference <- function(e) {
    n_theo = ((qnorm(C + (1 - C) / 2, 0, 1)^2 * p * (1 - p)) / e^2)
    fcf <- ifelse(is.infinite(N), 1, N / (N + n_theo - 1))
    n_adjusted <- n_theo * fcf
    return(abs(n_adjusted - n))
  }

  # Find the value of 'e' that minimizes the difference
  result <- optimize(f = difference, interval = c(0.001, 1))

  # Return the result containing the optimal (minimum) 'e' value
  return(result$minimum)
}



