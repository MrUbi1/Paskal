#' Confidence interval of the proportion under a Simple Random Sampling framework
#'
#' @param C Level of confidence (0 <= C <= 1)
#' @param e Sampling error (0 <= e <= 1)
#' @param n_real Real sample size (n_real > 0)
#' @param p_est Sample proportion (0 <= p_est <= 1)
#' @param p_exp Expected proportion in the population. By default 0.5 (0 <= p_exp <= 1)
#' @param parameter Type TRUE if you do know the populations sd, type FALSE (default) if it is an estimate.
#' @param N A positive integer indicating the number of elements in the population. By default, infinite.
#'
#' @return The function returns the interval of confidence of the population proportion, and information regarding the sufficiency of the sample size.
#' @export
#'
#' @examples cp_srs(C = 0.95, e = 0.05, p_exp = 0.3, n_real = 250, p_est = 0.4)
#' @examples cp_srs(C = 0.95, e = 0.05, p_exp = 0.3, n_real = 400, p_est = 0.4)
#' @examples cp_srs(C = 0.95, e = 0.05, p_exp = 0.3, n_real = 250, p_est = 0.4, parameter = TRUE, N = 5000)

#Confidence interval function
cp_srs <- function(C,e, p_exp = 0.5, n_real, p_est, parameter = FALSE, N = Inf) {

  # Check parameter ranges
  if (C < 0 || C > 1) {
    stop("Parameter 'C' must be in the range 0 <= C <= 1")
  }

  if (e < 0 || e > 1) {
    stop("Parameter 'e' must be in the range 0 <= e <= 1")
  }

  if (n_real != round(n_real) || n_real <= 0) {
    stop("Parameter 'n_real' must be a positive integer")
  }

  if (p_est < 0 || p_est > 1) {
    stop("Parameter 'p_est' must be in the range 0 <= p_est <= 1")
  }

  if (p_exp < 0 || p_exp > 1) {
    stop("Parameter 'p_exp' must be in the range 0 <= p_exp <= 1")
  }

  if (!missing(N)) {
    if (!is.infinite(N) && (N != round(N) || N <= 0)) {
      stop("Parameter 'N' must be a positive integer or Inf")
    }
  }

  # Calculate the confidence interval
  p_upper <- round(min(1, p_est + e),2)
  p_lower <- round(max(0, p_est - e), 2)
  inference = paste0("With ", C * 100, "% confidence, the proportion in the population is between ", p_lower, " and ", p_upper)
  cat(inference, "\n")

  # Calculus of needed sample size, given 'e'
  n_needed = ifelse(parameter == TRUE,
  qnorm(C + (1 - C) / 2, 0, 1)^2 * p_est * (1 - p_est) / e^2,
  qt(C + (1 - C) / 2, N)^2 * p_est * (1 - p_est) / e^2)

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
