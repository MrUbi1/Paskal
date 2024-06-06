#' Sampling error when estimating the total using a simple random sampling design
#'
#' @param C Level of confidence; 0 <= C <= 1.
#' @param n_real Real sample size; n_real > 0.
#' @param sd_est Expected standard deviation; sd_exp > 0.
#' @param N A positive integer representing the number of elements in the population.
#' @param parameter Type TRUE if you do know the population SD, or type FALSE (default) if it is an estimate.
#'
#' @return This function returns the sampling error when using a simple random sampling design without replacement to estimate the total, given the sample size.
#' @export
#'
#' @examples et_srs(C = 0.95, n_real = 87, sd_est = 4.1, N = 1200)
#' @examples et_srs(C = 0.95, n_real = 23, sd_est = 4.1, N = 1200)
#' @examples et_srs(C = 0.95, n_real = 6, sd_est = 2.1, N = 1200, parameter = TRUE)


# Sample error function
et_srs <- function(C, n_real, sd_est, N, parameter = FALSE) {

  # Check parameter ranges
  if (C < 0 || C > 1) {
    stop("Parameter 'C' must be in the range 0 <= C <= 1")
  }

  if (n_real <= 0 ) {
    stop("Parameter 'n_real' must be a positive integer")
  }

  if (sd_est < 0) {
    stop("Parameter 'sd_est' must be a positive number")
  }

  if (N != round(N) || N <= 0) {
    stop("Parameter 'N' must be a positive integer")
  }


  # Function of difference, aimed to iterate with different values of 'E'
  difference <- function(E) {
    n_theo <- ifelse(parameter == TRUE,
                     N^2 * qnorm(C + (1 - C) / 2, 0, 1)^2 * sd_est^2 / E^2, # qnorm: quantile of the normal distribution
                     N^2 * qt(C + (1 - C) / 2, N)^2 * sd_est^2 / E^2 # qt: quantile of the t-student distribution
                     )

    fcf <- ifelse(is.infinite(N), 1, N / (N + n_theo - 1))

    n_adjusted <- n_theo * fcf

    return(abs(n_adjusted - n_real))
  }

  # Find the value of 'E' that minimizes the difference
  result <- optimize(f = difference, interval = c(0, 1000000))

  # Return the result containing the optimal (minimum) 'E' value
  return(list(E = result$minimum))

}


