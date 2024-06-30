#' Sampling error when estimating the proportion using a simple random sampling design
#'
#' @param C Level of confidence; 0 <= C <= 1.
#' @param n_real Real sample size; n_real > 0.
#' @param p_est Sample proportion; 0 <= p_est <= 1.
#' @param N A positive integer representing the number of elements in the population. Defaults to infinite.
#'
#' @return This function returns the sampling error when using a simple random sampling design without replacement to estimate the proportion, given the sample size.
#' @export
#'
#' @examples ep_srs(C = 0.90, n_real = 415)
#' @examples ep_srs(C = 0.90, n_real = 415, p_est = 0.4)
#' @examples ep_srs(C = 0.90, n_real = 415, p_est = 0.4, N = 10000)
#' @examples ep_srs(C = 0.90, n_real = 415, N = 10000)

# Sample error function
ep_srs <- function(C, n_real, p_est = 0.5, N = Inf) {

  # Check parameter ranges
  if (C < 0 || C > 1) {
    stop("Parameter 'C' must be in the range 0 <= C <= 1")
  }

  if (!is.numeric(n_real) || n_real <= 0 || n_real != floor(n_real)) {
    stop("Parameter 'n_real' must be a positive integer")
  }


#  if (n_real != round(n_real) || n_real <= 0) {
#    stop("Parameter 'n_real' must be a positive integer")
#  }

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
    n_theo = qnorm(C + (1 - C) / 2, 0, 1)^2 * p_est * (1 - p_est) / e^2 # qnorm: quantile of the normal distribution

    fcf <- ifelse(is.infinite(N), 1, N / (N + n_theo - 1))

    n_adjusted <- n_theo * fcf

    return(abs(n_adjusted - n_real))
  }

  # Find the value of 'e' that minimizes the difference
  result <- optimize(f = difference, interval = c(0.001, 1))

  # Return the result containing the optimal (minimum) 'e' value
  return(list(e = result$minimum))

}



