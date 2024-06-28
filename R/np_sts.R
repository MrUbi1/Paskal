#' Sample size required to estimate the proportion using a stratified sampling design
#'
#' @param C Level of confidence; 0 <= C <= 1.
#' @param e Sampling error; 0 <= e <= 1.
#' @param p_exp A vector with the expected proportion in each stratum; 0 <= p_exp(i) <= 1, for every 'i' stratum.
#' @param alloc A vector with the relative allocation of sample size for each stratum; 0 < alloc(i) < 1, where sum(alloc(i)) = 1. If not defined (default), its values would be proportional to the size of each stratum.
#' @param N A vector of positive integers representing the number of elements in each stratum.
#'
#' @return This function returns the sample size required to estimate the proportion of occurrences of an event when using a stratified sampling design without replacement, given the level of risk.
#' @export
#'
#' @examples np_sts(C = 0.95, e = 0.1, p_exp = c(0.2, 0.3), N = c(220, 350))
#' @examples np_sts(C = 0.95, e = 0.1, p_exp = c(0.1, 0.5, 0.5), alloc = c(0.3, 0.3, 0.4), N = c(150, 40, 110))


# Sample size function
np_sts <- function(C, e, p_exp, alloc = NULL, N) {

  # Default allocation if 'alloc' is not provided
  if (is.null(alloc)) {
    alloc <- N / sum(N)
  }

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

  if (!all(alloc > 0)) {
    stop("All elements in 'alloc' must be greater than 0")
  }

  if (abs(sum(alloc) - 1) > .Machine$double.eps^0.5) {
    stop("The sum of elements in 'alloc' must be equal to 1")
  }

  if (!all(N == round(N) & N > 0)) {
    stop("All elements in 'N' must be positive integers")
  }

  if (length(p_exp) != length(alloc) || length(p_exp) != length(N)) {
    stop("'p_exp', 'alloc', and 'N' must have the same length")
  }

  # Formula to obtain the adjusted sample size (Ref. 5.15)
  Z <- qnorm(C + (1 - C) / 2, 0, 1) # qnorm: quantile of the normal distribution
  n = sum(N^2 * p_exp * (1 - p_exp) / alloc) / (sum(N)^2 * e^2 / Z^2 + sum(N * p_exp * (1 - p_exp)))

  n_adjusted_i <- ceiling(n * alloc)

  return(list(n = sum(n_adjusted_i), n_i = n_adjusted_i))

}

#Dudas
# Tiene sentido que el muestreo estratificado sea con N infinito? Qué pasa si no tenemos N, por ejemplo, al extraer muestras de estratos geológicos?
# Ojo que si hay más de un estrato y las alocaciones son distintas al proporcional, con e cercano 0 se da que n > N, por ejemplo:
C <- 0.95
e <- 0.1
p_exp <- c(0.7, 0.95)
alloc = c(0.1, 0.9)
N <- c(450, 50)
np_sts(C, e, p_exp, alloc, N)

