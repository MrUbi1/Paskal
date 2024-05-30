#' Sampling error when estimating the proportion with a Stratified Sampling plan
#'
#' @param C Level of confidence (0 <= C <= 1)
#' @param n_real Given sample size (n_real > 0)
#' @param p_est Sample proportion (0 <= p_est <= 1).
#' @param parameter Type TRUE if you do know the populations SD, type FALSE (default) if it is an estimate. # ###REVISAR
#' @param N A positive integer indicating the number of elements in each strata. # ###REVISAR
#'
#' @return The function returns the sample error consistent with the estimation of the proportion of occurrence of a phenomena, given the sample size.
#' @export
#'
#' @examples ep_sts(C = 0.95, n_real = c(66, 9, 9), p_est = c(0.2, 0.5, 0.7), alloc = c(0.8, 0.1, 01), parameter = TRUE, N = c(1400, 400, 200))


# Sample error function for stratified sampling
ep_sts <- function(C, n_real, p_est, alloc = NULL, parameter = FALSE, N) {

  # Check parameter ranges
  if (C < 0 || C > 1) {
    stop("Parameter 'C' must be in the range 0 <= C <= 1")
  }

  if (any(p_est < 0 | p_est > 1)) {
    stop("All elements in 'p_est' must be in the range 0 <= p_est <= 1")
  }

  #  if (!missing(N)) {
  #    if (!is.infinite(N) && (any(N != round(N)) || any(N <= 0))) {
  #      stop("All elements in 'N' must be positive integers or Inf")
  #    }
  #  }

  # Default allocation if 'alloc' is not provided
  if (is.null(alloc)) {
    alloc <- N / sum(N)
  }

  # Ensure 'p_est', 'alloc', and 'N' are of the same length
  if (length(p_est) != length(alloc) ||
      length(p_est) != length(N)) {
    stop("'p_est', 'alloc', and 'N' must have the same length")
  }

  # Function of difference, aimed to iterate with different values of 'e'
  difference <- function(e) {
    n_adjusted <- if (parameter) {
      Z <- qnorm(C + (1 - C) / 2, 0, 1)
      sum(N^2 * p_est * (1 - p_est) / alloc) /
        (sum(N)^2 * e^2 / Z^2 + sum(N * p_est * (1 - p_est)))

    } else {
      t <- qt(C + (1 - C) / 2, sum(n_real) - 1)
      sum(N^2 * p_est * (1 - p_est) / alloc) /
        (sum(N)^2 * e^2 / t^2 + sum(N * p_est * (1 - p_est)))
    }

    return(sum(n_adjusted * alloc - n_real))

  }

  # Find the value of 'e' that minimizes the difference
  result <- optimize(f = difference, interval = c(0.001, 1))

  # Return the result containing the optimal (minimum) 'e' value
  return(result$minimum)
}
