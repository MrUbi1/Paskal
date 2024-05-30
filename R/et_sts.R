#' Sampling error when estimating the total with a Stratified Sampling plan
#'
#' @param C Level of confidence (0 <= C <= 1)
#' @param n_real Given sample size (n_real > 0)
#' @param p_est Sample proportion (0 <= p_est <= 1).
#' @param parameter Type TRUE if you do know the populations SD, type FALSE (default) if it is an estimate.
#' @param N A positive integer indicating the number of elements in each strata. # ###REVISAR
#'
#' @return The function returns the sample error consistent with the estimation of the total of a phenomena, given the sample size.
#' @export
#'
#' @examples et_sts(C = 0.95, n_real = c(48, 14, 7), sd_est = c(5, 15, 10), alloc = c(1/2, 1/4, 1/4), parameter = TRUE, N = c(155, 62, 93))

# Sample error function for stratified sampling
et_sts <- function(C, n_real, sd_est, alloc = NULL, parameter = FALSE, N) {

  # Check parameter ranges
  if (C < 0 || C > 1) {
    stop("Parameter 'C' must be in the range 0 <= C <= 1")
  }

  if (any(sd_est < 0)) {
    stop("All elements in 'sd_est' must be positive numbers")
  }

  # Default allocation if 'alloc' is not provided
  if (is.null(alloc)) {
    alloc <- N / sum(N)
  }

  # Ensure 'sd_est', 'alloc', and 'N' are of the same length
  if (length(sd_est) != length(alloc) || length(sd_est) != length(N) || length(sd_est) != length(n_real)) {
    stop("'sd_est', 'alloc', 'N', and 'n_real' must have the same length")
  }



  # Function of difference, aimed to iterate with different values of 'E'
  difference <- function(E) {
    n_adjusted <- if (parameter) {
      Z <- qnorm(C + (1 - C) / 2, 0, 1)
      sum(N^2 * sd_est^2 / alloc) /
        ((sum(N)^2 * E^2 / (Z^2 * sum(N)^2)) + sum(N * sd_est^2))
    } else {
      t <- qt(C + (1 - C) / 2, sum(n_real) - 1)
      sum(N^2 * sd_est^2 / alloc) /
        ((sum(N)^2 * E^2 / (t^2 * sum(N)^2)) + sum(N * sd_est^2))
    }

    return(abs(sum(n_adjusted * alloc - n_real)))
  }



  # Find the value of 'E' that minimizes the difference
  result <- optimize(f = difference, interval = c(0.001, 1000000))

  # Return the result containing the optimal (minimum) 'E' value
  E <- result$minimum
  n_adjusted <- if (parameter) {
    Z <- qnorm(C + (1 - C) / 2, 0, 1)
    sum(N^2 * sd_est^2 / alloc) /
      ((sum(N)^2 * E^2 / (Z^2 * sum(N)^2)) + sum(N * sd_est^2))
  } else {
    t <- qt(C + (1 - C) / 2, sum(n_real) - 1)
    sum(N^2 * sd_est^2 / alloc) /
      ((sum(N)^2 * E^2 / (t^2 * sum(N)^2)) + sum(N * sd_est^2))
  }

  return(list(E = E, n_real = n_real, n_adjusted = ceiling(n_adjusted * alloc), diff = sum(n_adjusted * alloc - n_real)))
}
