#' Sampling error when estimating the total with a Simple Random Sampling plan
#'
#' @param C Level of confidence (0 <= C <= 1)
#' @param n_real Given sample size (n_real > 0)
#' @param sd_exp Expected standard deviation (sd_exp > 0)
#' @param parameter Type TRUE if you do know the populations sd, type FALSE (default) if it is an estimate.
#' @param N A positive integer indicating the number of elements in the population.
#'
#' @return The function returns the sample error consistent with the estimation of the total, given the sample size.
#' @export
#'
#' @examples ex_srs(C = 0.95, n_real = 87, sd_exp = 4.1, N = 1200)
#' @examples ex_srs(C = 0.95, n_real = 23, sd_exp = 4.1, N = 1200)
#' @examples ex_srs(C = 0.95, n_real = 6, sd_exp = 2.1, parameter = TRUE, N = 1200)


# Sample error function
et_srs <- function(C, n_real, sd_exp, parameter = FALSE, N) {

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

  if (N != round(N) || N <= 0) {
    stop("Parameter 'N' must be a positive integer or Inf")
  }


  # Function of difference, aimed to iterate with different values of 'E'
  difference <- function(E) {
    n_theo <- ifelse(parameter == TRUE,
                     N^2 * qnorm(C + (1 - C) / 2, 0, 1)^2 * sd_exp^2 / E^2,
                     N^2 * qt(C + (1 - C) / 2, N)^2 * sd_exp^2 / E^2
                     )

    fcf <- ifelse(is.infinite(N), 1, N / (N + n_theo - 1))

    n_adjusted <- n_theo * fcf

    return(abs(n_adjusted - n_real))
  }

  # Find the value of 'E' that minimizes the difference
  result <- optimize(f = difference, interval = c(0, 1000000))

  # Return the result containing the optimal (minimum) 'E' value
  return(result$minimum)
}


