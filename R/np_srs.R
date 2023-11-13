#' Sample size n for simple random sampling of proportion
#'
#' @param C Level of confidence (0 <= C <= 1)
#' @param p Expected proportion in the population. By default 0.5, also applicable if you lack information. (0 <= p <= 1)
#' @param e Sampling error (0 <= e <= 1).
#' @param N Population size, by default is infinite. Must be a positive integer.
#'
#' @return The function returns the sample size needed to estimate the proportion of occurrende of a phenomena, consistent with the risk ('C' and 'e') that the auditor is willing to assume.
#' @export
#'
#' @examples np_srs(0.95, 0.03)
#' @examples np_srs(0.95, 0.03, 0.5)
#' @examples np_srs(0.95, 0.03, 0.5, 1000)
#' @examples np_srs(0.95, 0.03, N = 1000)


np_srs <- function(C, e, p = 0.5, N = Inf) {
  if (C < 0 || C > 1) {
    stop("Parameter 'C' must be in the range 0 <= C <= 1")
  }

  if (p < 0 || p > 1) {
    stop("Parameter 'p' must be in the range 0 <= p <= 1")
  }

  if (e < 0 || e > 1) {
    stop("Parameter 'e' must be in the range 0 <= e <= 1")
  }

  if (missing(N)) {
    # Si N está ausente, no hagas nada y continúa ejecutando
  } else {
    if (N != round(N) || N <= 0) {
      stop("Parameter 'N' must be a positive integer")
    }
    # Continúa con el resto del código si N es un número entero positivo
  }

  n <- qnorm(C + (1 - C) / 2, 0, 1)^2 * p * (1 - p) / e^2
  fcf <- ifelse(is.infinite(N), 1, N / (N + n - 1))
  n_ajustado <- ceiling(n * fcf)
  return(n_ajustado)
}



