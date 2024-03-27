#' Confidence interval of the mean under a Simple Random Sampling framework
#'
#' @param C Level of confidence. (0 <= C <= 1)
#' @param E Sampling error. (E > 0)
#' @param sd_exp Expected standard deviation (s_exp > 0)
#' @param sd_est Sample standar deviation (s_est > 0)
#' @param n_real Real sample size (n_real > 0)
#' @param x_est Sample mean
#' @param parameter Type TRUE if you do know the populations sd, type FALSE (default) if it is an estimate.
#' @param N A positive integer indicating the number of elements in the population. By default, infinite.
#'
#' @return The function returns the interval of confidence of the population mean, and information regarding the sufficiency of the sample size.
#' @export
#'
#' @examples cx_srs(C = 0.95, E = 50, x_est = 3500, sd_est = 200, n_real = 250, sd_exp = 200)
#' @examples cx_srs(C = 0.95, E = 50, x_est = 3500, sd_est = 400, n_real = 400, sd_exp = 200)
#' @examples cx_srs(C = 0.95, E = 50, x_est = 3500, sd_est = 400, n_real = 250, sd_exp = 200, parameter = TRUE, N = 5000)

#Confidence interval function
cx_srs <- function(C, E, x_est, sd_exp, n_real, sd_est, parameter = FALSE, N = Inf) {

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

  if (!missing(N)) {
    if (!is.infinite(N) && (N != round(N) || N <= 0)) {
      stop("Parameter 'N' must be a positive integer or Inf")
    }
  }

  # Calculate the confidence interval

  fcf <- ifelse(is.infinite(N), 1, ((N - n_real) / (N - 1)))

  sd_x_est <- sd_est / sqrt(n_real) * sqrt(fcf)

  LP <- ifelse(parameter == TRUE, qnorm(C + (1 - C) / 2, 0, 1), qt(C + (1 - C) / 2, N)) * sd_x_est

  p_upper <- round(x_est + LP, 3)

  p_lower <- round(x_est - LP, 3)

  inference <- paste0("With ", C * 100, "% confidence, the population mean is between ", p_lower, " and ", p_upper)

  cat(inference, "\n")

  # Calculus of needed sample size, given 's_est'
  n_needed = ifelse(parameter == TRUE,
                    qnorm(C + (1 - C) / 2, 0, 1)^2 * sd_exp^2 / E^2,
                    qt(C + (1 - C) / 2, N)^2 * sd_exp^2 / E^2
                    )

  fcf_needed <- ifelse(is.infinite(N), 1, N / (N + n_needed - 1))

  n_needed_adj <- ceiling(n_needed * fcf_needed)

  compare_sample_sizes(n_real, n_needed_adj)
}

compare_sample_sizes <- function(n_real, n_needed_adj) {
  if (n_real < n_needed_adj) {
    cat("Your sample size:", n_real, "\n")
    cat("Needed sample size:", n_needed_adj, "\n")
    cat("Consider increasing the sample size if you want to reduce the width of the interval\n")
  } else {
    cat("Your sample size:", n_real, "\n")
    cat("Needed sample size:", n_needed_adj, "\n")
    cat("Your sample size seems to be sufficient.\n")
  }
}



