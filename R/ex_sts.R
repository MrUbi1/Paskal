#' Sampling error when estimating the mean using a stratified sampling design
#'
#' @param C Level of confidence; 0 <= C <= 1.
#' @param n_real A vector of positive integers representing the real sample size of each stratum.
#' @param sd_est A vector with the estimated standard deviation in each stratum; sd_est(i) > 0, for every 'i' stratum.
#' @param alloc A vector with the relative allocation of sample size for each stratum; 0 < alloc(i) < 1, where sum(alloc(i)) = 1. If not defined (default), its values would be proportional to the size of each stratum.
#' @param N A vector of positive integers representing the number of elements in each stratum; N(i) > 0.
#' @param parameter Type TRUE if you do know the populations SD in sd_exp, or type FALSE (default) if they are estimates.
#'
#' @return This function returns the global sampling error when using a stratified sampling design without replacement to estimate the mean, given the sample size.
#' @export
#'
#' @examples ex_sts(C = 0.95, n_real = c(48, 14, 7), sd_est = c(0.2, 0.5, 0.7), alloc =c(0.7, 0.2, 0.1), N = c(1400, 400, 200), parameter = TRUE)


# Sample error function
ex_sts <- function(C, n_real, sd_est, alloc = NULL, N, parameter = FALSE) {

  # Default allocation if 'alloc' is not provided
  if (is.null(alloc)) {
    alloc <- N / sum(N)
  }

  # Check parameter ranges
  if (C < 0 || C > 1) {
    stop("Parameter 'C' must be in the range 0 <= C <= 1")
  }

  if (any(n_real < 0)) {
    stop("All elements in 'n_real' must be greater than 0")
  }

  if (any(sd_est < 0)) {
    stop("All elements in 'sd_est' must be positive numbers")
  }

  if (any(alloc <= 0 | alloc > 1)) {
    stop("All elements in 'alloc' must be in the range 0 < alloc <= 1")
  }

  if (abs(sum(alloc) - 1) > .Machine$double.eps^0.5) {
    stop("The sum of elements in 'alloc' must be equal to 1")
  }

  # Ensure 'n_real', 'sd_est', 'alloc', and 'N' are of the same length
  if (length(sd_est) != length(alloc) ||
      length(sd_est) != length(N) ||
      length(sd_est) != length(n_real)) {
    stop("'n_real', 'sd_est', 'alloc', and 'N' must have the same length")
  }

  # Function of difference, aimed to iterate with different values of 'E' (Ref. 5.6)
  difference <- function(E) {
    n_adjusted <- if (parameter) {
      Z <- qnorm(C + (1 - C) / 2, 0, 1) # qnorm: quantile of the normal distribution
      sum(N^2 * sd_est^2 / alloc) /
        (sum(N)^2 * E^2 / Z^2 + sum(N * sd_est^2))

    } else {
      t <- qt(C + (1 - C) / 2, sum(n_real) - 1) # qt: quantile of the t-student distribution
      sum(N^2 * sd_est^2 / alloc) /
        (sum(N)^2 * E^2 / t^2 + sum(N * sd_est^2))
    }

    return(abs(sum(n_adjusted * alloc - n_real)))
  }


  # Find the value of 'E' that minimizes the difference
  result <- optimize(f = difference, interval = c(0.001, 1000000))

# Desde aquí se puede eliminar
  # Return the result containing the optimal (minimum) 'E' value
  E_min <- result$minimum
  n_adjusted <- if (parameter) {
    Z <- qnorm(C + (1 - C) / 2, 0, 1) # qnorm: quantile of the normal distribution
    sum(N^2 * sd_est^2 / alloc) /
      (sum(N)^2 * E_min^2 / Z^2 + sum(N * sd_est^2))

  } else {
    t <- qt(C + (1 - C) / 2, sum(n_real) - 1) # qt: quantile of the t-student distribution
    sum(N^2 * sd_est^2 / alloc) /
      (sum(N)^2 * E_min^2 / t^2 + sum(N * sd_est^2))
  }
# Hasta aquí (una vez chequeadas las funciones)

  return(list(E = result$minimum)) # other options: n_optimized = ceiling(n_adjusted * alloc), diff = sum(n_adjusted * alloc - n_real)
}

#Dudas
# Dejo o elimino la verificación? (ver leyenda '#Desde aquí...'). Valido para
