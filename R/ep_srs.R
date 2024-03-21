#' Sampling error when estimating the proportion with a Simple Random Sampling plan
#'
#' @param C Level of confidence (0 <= C <= 1)
#' @param n_real Given sample size (n_real > 0)
#' @param p_est Sample proportion (0 <= p_est <= 1).
#' @param N A positive integer indicating the number of elements in the population. By default, infinite.
#'
#' @return The function returns the sample error consistent with the estimation of the proportion of occurrence of a phenomena, given the sample size.
#' @export
#'
#' @examples ep_srs(C = 0.90, n_real = 415)
#' @examples ep_srs(C = 0.90, n_real = 415, p_est = 0.5)
#' @examples ep_srs(C = 0.90, n_real = 415, p_est = 0.5, N = 20000)
#' @examples ep_srs(C = 0.90, n_real = 415, N = 20000)

# Sample error function
ep_srs <- function(C, n_real, p_est = 0.5, N = Inf) {

  # Check parameter ranges
  if (C < 0 || C > 1) {
    stop("Parameter 'C' must be in the range 0 <= C <= 1")
  }

  if (n_real <= 0 ) {
    stop("Parameter 'n_real' must be a positive integer")
  }

  if (p_est < 0 || p_est > 1) {
    stop("Parameter 'p_est' must be in the range 0 <= p_est <= 1")
  }

  if (!missing(N)) {
    if (!is.infinite(N) && (N != round(N) || N <= 0)) {
      stop("Parameter 'N' must be a positive integer or Inf")
    }
  }

  # Function of difference, aimed to iterate with different values of 'e'
  difference <- function(e) {
    n_theo = ((qnorm(C + (1 - C) / 2, 0, 1)^2 * p_est * (1 - p_est)) / e^2)
    fcf <- ifelse(is.infinite(N), 1, N / (N + n_theo - 1))
    n_adjusted <- n_theo * fcf
    return(abs(n_adjusted - n_real))
  }

  # Find the value of 'e' that minimizes the difference
  result <- optimize(f = difference, interval = c(0.001, 1))

  # Return the result containing the optimal (minimum) 'e' value
  return(result$minimum)
}



