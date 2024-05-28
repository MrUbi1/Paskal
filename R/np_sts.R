#' Sample size needed to estimate the proportion with a Stratified Sampling plan
#'
#' @param C Level of confidence (0 <= C <= 1).
#' @param e Sampling error (0 <= e <= 1).
#' @param p_exp Expected proportion in each stratum (0 <= p_exp <= 1).
#' @param alloc Relative allocation of sample size in each stratum (0 <= alloc <= 1). If not defined (default), its values would be the relative weight of each stratum, N_i / N.
#' @param parameter Type TRUE if you do know the populations SD, type FALSE (default) if it is an estimate. # ###REVISAR
#' @param N A positive integer indicating the number of elements in each strata. # ###REVISAR
#'
#' @return The function returns the sample size needed to estimate the proportion of occurrence of a phenomena, consistent with the risk the auditor is willing to assume, when conducting a Stratified Sampling plan.
#' @export
#'
#' @examples np_sts(C = 0.95, e = 0.1, p_exp = c(0.2, 0.3), parameter = TRUE, N = c(220, 350))
#' @examples np_sts(C = 0.95, e = 0.1, p_exp = c(0.1, 0.5, 0.5), parameter = TRUE, N = c(155, 62, 93))


# Consultas para Marcela
# Tiene sentido que el muestreo estratificado sea con N infinito?
# Tiene sentido usar la distribición 'Z' para la proporción? Digo, porque nunca se sabe la SD de la población (p x (1-p)), de otro modo no se estimaría 'p'.

# Pendientes para mi
# Ver por qué, si hay más de un estrato y las alocaciones son distintas al proporcional, con e = 0 se da que n > N
# Qué pasa si no tenemos N, por ejemplo, al extraer muestras de estratos geológicos?



# Sample size function

np_sts <- function(C, e, p_exp, alloc = NULL, parameter = FALSE, N) {

  # Check parameter ranges
  if (C < 0 || C > 1) {
    stop("Parameter 'C' must be in the range 0 <= C <= 1")
  }

  if (e < 0 || e > 1) {
    stop("Parameter 'e' must be in the range 0 <= e <= 1")
  }

  if (!all(p_exp >= 0 & p_exp <= 1)) {
    stop("All elements in 'p_exp' must be in the range 0 <= p_exp <= 1")
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

  # Ensure 'p_exp', 'alloc', and 'N' are of the same length
  if (length(p_exp) != length(alloc) || length(p_exp) != length(N)) {
    stop("'p_exp', 'alloc', and 'N' must have the same length")
  }

  # Formula to obtain the adjusted sample size
  n <- if (parameter) {
    Z <- qnorm(C + (1 - C) / 2, 0, 1)
    sum(N^2 * p_exp * (1 - p_exp) / alloc) /
      (sum(N)^2 * e^2 / Z^2 + sum(N * p_exp * (1 - p_exp)))

  } else {
    t <- qt(C + (1 - C) / 2, sum(N) - 1)
    sum(N^2 * p_exp * (1 - p_exp) / alloc) /
      (sum(N)^2 * e^2 / t^2 + sum(N * p_exp * (1 - p_exp)))
  }

  n_adjusted_i <- ceiling(n * alloc)

  return(list(n = sum(n_adjusted_i), n_i = n_adjusted_i))

}
