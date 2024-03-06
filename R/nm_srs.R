#' Sample size n for simple random sampling of mean
#'
#' @param C Level of confidence (0 <= C <= 1)
#' @param e Sampling error (0 <= e <= 1).
#' @param s To be completed
#' @param N Population size, by default is infinite. Must be a positive integer.
#'
#' @return The function returns the sample size needed to estimate the mean of a phenomena, consistent with the risk ('C' and 'e') that the auditor is willing to assume.
#' @export
#'
#' @examples np_srs(0.95, 0.03)
#' @examples np_srs(0.95, 0.03, 0.5)
#' @examples np_srs(0.95, 0.03, 0.5, 1000)
#' @examples np_srs(0.95, 0.03, N = 1000)


nm_srs <- function(C, e, s, N = Inf) {
  if (C < 0 || C > 1) {
    stop("Parameter 'C' must be in the range 0 <= C <= 1")
  }

  if (s < 0) {
    stop("Parameter 's' must be a positive integer")
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

  n <- qnorm(C + (1 - C) / 2, 0, 1)^2 * s^2
  fcf <- ifelse(is.infinite(N), 1, N / (N + n - 1))
  n_ajustado <- ceiling(n * fcf)
  return(n_ajustado)
}

