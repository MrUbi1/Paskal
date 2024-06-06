#' Confidence interval of the proportion under a simple random sampling design
#'
#' @param C Level of confidence; 0 <= C <= 1.
#' @param p_est Sample proportion; 0 <= p_est <= 1.
#' @param n_real Real sample size; n_real > 0.
#' @param N A positive integer indicating the number of elements in the population. Defaults to infinite.
#'
#' @return The function returns the confidence interval of the population proportion.
#' @export
#'
#' @examples cp_srs(C = 0.95, n_real = 250, p_est = 0.4)
#' @examples cp_srs(C = 0.95, n_real = 400, p_est = 0.4)
#' @examples cp_srs(C = 0.95, n_real = 250, p_est = 0.4, N = 5000)

#Confidence interval function
cp_srs <- function(C,n_real, p_est, N = Inf) {

  # Check parameter ranges
  if (C < 0 || C > 1) {
    stop("Parameter 'C' must be in the range 0 <= C <= 1")
  }

  if (n_real != round(n_real) || n_real <= 0) {
    stop("Parameter 'n_real' must be a positive integer")
  }

  if (p_est < 0 || p_est > 1) {
    stop("Parameter 'p_est' must be in the range 0 <= p_est <= 1")
  }

  if (!missing(N)) {
    if (!is.infinite(N) && (N != round(N) || N <= 0)) {
      stop("Parameter 'N' must be a positive integer or Inf")
    }
  }

  # Calculate the confidence interval

  fcf <- ifelse(is.infinite(N), 1, ((N - n_real) / (N - 1)))

  sd_p_est <- sqrt((p_est * (1 - p_est) / (n_real - 1)) * fcf)

  LP <- qnorm(C + (1 - C) / 2, 0, 1) * sd_p_est # qnorm: quantile of the normal distribution

  p_upper <- round(min(1, p_est + LP), 3)

  p_lower <- round(max(0, p_est - LP), 3)

  inference <- paste0("The population proportion is between ", p_lower, " and ", p_upper, " with ", C * 100, "% confidence.")

  return(list(p_est = p_est, margin_of_error = LP, inference = inference))

}

