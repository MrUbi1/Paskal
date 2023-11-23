#' Sample error e for simple random sampling of proportion
#'
#' @param C Level of confidence (0 <= C <= 1)
#' @param n Given sample size
#' @param p Expected or estimated (preferred) proportion in the population.
#' @param N Population size. Must be a positive integer.
#'
#' @return The function returns the sample error consistent with the estimation of the proportion of occurrende of a phenomena, given the sample size.
#' @export
#'
#' @examples np_srs(0.95, 151, 0.5, 9999999999)


ep_srs <- function(C, n, p, N = Inf) {
  if (C < 0 || C > 1) {
    stop("Parameter 'C' must be in the range 0 <= C <= 1")
  }

  if (p < 0 || p > 1) {
    stop("Parameter 'p' must be in the range 0 <= p <= 1")
  }

  if (n <= 0 ) {
    stop("Parameter 'n' must be higher than 0")
  }

  if (missing(N)) {
    # Si N está ausente, no hagas nada y continúa ejecutando
  } else {
    if (N != round(N) || N <= 0) {
      stop("Parameter 'N' must be a positive integer")
    }
    # Continúa con el resto del código si N es un número entero positivo
  }

  diferencia <- function(e) {
    n_infinite <- ((qnorm(C + (1 - C) / 2, 0, 1)^2 * p * (1 - p)) / e^2) * N / (N + ((qnorm(C + (1 - C) / 2, 0, 1)^2 * p * (1 - p)) / e^2) - 1)

    # Evitar división por cero
    if (e == 0) {
      return(Inf)
    }

    return(abs(n_infinite - n))
  }

  # Asegúrate de que C, p, y e estén definidos antes de llamar a qnorm
  if (!missing(C) && !missing(p) && !missing(n)) {
    # Encontrar el valor de e que minimiza la diferencia
    resultado <- optimize(f = diferencia, interval = c(0.001, 1))

    # Devolver el resultado que contiene el valor de e óptimo
    return(resultado$minimum)
  } else {
    # Si falta algún parámetro necesario, puedes hacer algo más o devolver un valor predeterminado
    return(some_default_value)
  }
}



