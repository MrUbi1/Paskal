#' Confidence interval of the proportion from a Simple Random Sampling plan
#'
#' @param C Level of confidence. (0 <= C <= 1)
#' @param e Sampling error. (0 <= e <= 1).
#' @param n_real Real sample size. (n > 0).
#' @param p_est Sample proportion. (0 <= p <= 1)
#' @param p_exp Expected proportion in the population. By default 0.5. (0 <= p <= 1)
#' @param N Population size, by default is infinite. Must be a positive integer.
#'
#' @return The function returns the sample size needed to estimate the proportion of occurrence of a phenomena, consistent with the risk ('C' and 'e') that the auditor is willing to assume, when conducting a Simple Random Sampling plan, among others with some restrictions.
# @export
#'
#' @examples np_srs(0.90, 0.04)
#' @examples np_srs(0.90, 0.04, 0.5)
#' @examples np_srs(0.90, 0.04, 0.5, 20000)
#' @examples np_srs(0.90, 0.04, N = 20000)

# Confidence function
sample_analysis <- function(C, e, n_real, p_est, p_exp = 0.5, N = Inf) {

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
inference <- paste0("With ",C * 100,"% confidence, the population proportion is between ", max(0, p_est - e), " and ", p_est + e)
print(inference)

# Compare theoretical vs needed sample size
n_needed <- qnorm(C + (1 - C) / 2, 0, 1)^2 * p_est * (1 - p_est) / e^2
fcf_needed <- ifelse(is.infinite(N), 1, N / (N + n_needed - 1))
n_needed_adj <- ceiling(n_needed * fcf_needed)

compare_sample_sizes <- function(n_real, n_needed_adj) {
  if (n_real < n_needed_adj) {
    print(paste("Your sample size:",n_real))
    print(paste("Needed sample size (given 'p_est'):",n_needed_adj))
    print(paste("Consider increasing the sample size"))

  } else {
    print(paste("Your sample size:",n_real))
    print(paste("Needed sample size (given 'p_est'):",n_needed_adj))
    print("Your sample size seems to sufficient.")
  }
}

compare_sample_sizes(n_real, n_needed_adj)
}
