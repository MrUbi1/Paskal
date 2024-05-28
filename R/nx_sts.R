#' Sample size needed to estimate the mean with a Stratified Sampling plan
#'
#' @param C Level of confidence (0 <= C <= 1).
#' @param E Sampling error (E > 0).
#' @param sd_exp Expected standard deviation (sd_exp > 0)
#' @param alloc Relative allocation of sample size in each stratum (0 <= alloc <= 1). If not defined (default), its values would be the relative weight of each stratum, N_i / N.
#' @param parameter Type TRUE if you do know the populations sd, type FALSE (default) if it is an estimate.
#' @param N A positive integer indicating the number of elements in the population. By default, infinite. # ###REVISAR
#'
#' @return The function returns the sample size needed to estimate the mean of a phenomena, consistent with the risk ('C' and 'e') that the auditor is willing to assume.
#' @export
#'
#' @examples nx_sts(C = 0.95, E = 2, sd_exp = c(5, 15), parameter = TRUE, N = c(220, 350))
#' @examples nx_sts(C = 0.95, E = 2, sd_exp = c(5, 15, 10), parameter = FALSE, alloc = c(0.4, 0.2, 0.4), N = c(155, 62, 93))


# Consultas para Marcela
# Tiene sentido que el muestreo estratificado sea con N infinito?

# Pendientes para mi




# Sample size function

nx_sts <- function(C, E, sd_exp, alloc = NULL, parameter = FALSE, N) {

  # Check parameter ranges
  if (C < 0 || C > 1) {
    stop("Parameter 'C' must be in the range 0 <= C <= 1")
  }

  if (E <= 0) {
    stop("Parameter 'E' must be a positive integer")
  }

  if (!all(sd_exp >= 0)) {
    stop("All elements in 'sd_exp' must be positive integers")
  }

  # Default allocation if 'alloc' is not provided
  if (is.null(alloc)) {
    alloc <- N / sum(N)
  }

  if (!all(alloc > 0)) {
    stop("All elements in 'alloc' must be greater than 0")
  }

  if (!all(is.infinite(N) | (N == round(N) & N > 0))) { # ###REVISAR
    stop("All elements in 'N' must be positive integers or Inf")
  }

  # Ensure 'sd_exp', 'alloc', and 'N' are of the same length
  if (length(sd_exp) != length(alloc) || length(sd_exp) != length(N)) {
    stop("'sd_exp', 'alloc', and 'N' must have the same length")
  }

  # Formula to obtain the adjusted sample size
  n <- if (parameter) {

    Z <- qnorm(C + (1 - C) / 2, 0, 1)

    sum(N^2 * sd_exp^2 / alloc) /
      (sum(N)^2 * E^2 / Z^2 + sum(N * sd_exp^2))

  } else {

    t <- qt(C + (1 - C) / 2, sum(N) - 1)

    sum(N^2 * sd_exp^2 / alloc) /
      (sum(N)^2 * E^2 / t^2 + sum(N * sd_exp^2))
  }

  n_adjusted_i <- ceiling(n * alloc)

  return(list(n = sum(n_adjusted_i),
              n_i = n_adjusted_i

              ))

}
