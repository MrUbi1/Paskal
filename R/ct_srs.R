#' Confidence interval of the total under a Simple Random Sampling framework
#'
#' @param C Level of confidence. (0 <= C <= 1)
#' @param E Sampling error. (E > 0)
#' @param n_real Real sample size (n_real > 0)
#' @param t_est Total estimated from the sample
#' @param sd_est Sample standar deviation (sd_est > 0)
#' @param sd_exp Expected standard deviation (sd_exp > 0)
#' @param N A positive integer indicating the number of elements in the population.
#'
#' @return The function returns the interval of confidence of the population total, and information regarding the sufficiency of the sample size.
#' @export
#'
#' @examples ct_srs(C = 0.95, E = 1000, t_est = 3500, sd_exp = 4.1, n_real = 87, sd_est = 4.1, N = 1200)
#' @examples ct_srs(C = 0.95, E = 1000, t_est = 3500, sd_exp = 4.1, n_real = 87, sd_est = 6, N = 1200)


nt_srs(C = 0.95, E = 1000, sd_exp = 4.1, N = 1200)87


#Confidence interval function
ct_srs <- function(C, E, t_est, sd_exp, n_real, sd_est, N) {

  # Check parameter ranges
  if (C < 0 || C > 1) {
    stop("Parameter 'C' must be in the range 0 <= C <= 1")
  }

  if (E < 0) {
    stop("Parameter 'E' must be a positive number")
  }

  if (n_real != round(n_real) || n_real <= 0) {
    stop("Parameter 'n_real' must be a positive integer")
  }

  if (sd_est < 0) {
    stop("Parameter 'sd_est' must be a positive number")
  }

  if (sd_exp < 0) {
    stop("Parameter 'sd_exp' must be a positive number")
  }

  if (N != round(N) || N <= 0) {
    stop("Parameter 'N' must be a positive integer or Inf")
  }


  # Calculate the confidence interval
  p_upper <- round(t_est + E, 2)
  p_lower <- round(t_est - E, 2)
  inference = paste0("With ", C * 100, "% confidence, the the population mean is between ", p_lower, " and ", p_upper)
  cat(inference, "\n")

  # Calculus of needed sample size, given 's_est'
  n_needed <- N^2 * qnorm(C + (1 - C) / 2, 0, 1)^2 * sd_est^2 / E^2
  fcf_needed <- ifelse(is.infinite(N), 1, N / (N + n_needed - 1))
  n_needed_adj <- ceiling(n_needed * fcf_needed)

  compare_sample_sizes(n_real, n_needed_adj)
}

compare_sample_sizes <- function(n_real, n_needed_adj) {
  if (n_real < n_needed_adj) {
    cat("Your sample size:", n_real, "\n")
    cat("Needed sample size:", n_needed_adj, "\n")
    cat("Consider increasing the sample size\n")
  } else {
    cat("Your sample size:", n_real, "\n")
    cat("Needed sample size:", n_needed_adj, "\n")
    cat("Your sample size seems to be sufficient.\n")
  }
}


#En el cálculo del intervalo de confianza, p_upper y p_lower se calculan sumando y restando E a t_est.
#Esto puede no ser correcto si t_est representa la media muestral. Por lo general, se utiliza la fórmula t_est ± E * (sd / sqrt(n))
#para calcular el intervalo de confianza para la media. Asegúrate de utilizar la fórmula correcta para el cálculo del intervalo de confianza.


