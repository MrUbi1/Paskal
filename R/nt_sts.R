#' Sample size required to estimate the total using a stratified sampling design
#'
#' @param C Level of confidence; 0 <= C <= 1.
#' @param E Sampling error; E > 0.
#' @param sd_exp A vector with the expected standard deviation in each stratum; sd_exp(i) > 0, for every 'i' stratum.
#' @param alloc A vector with the relative allocation of sample size for each stratum; 0 < alloc(i) < 1, where sum(alloc(i)) = 1. If not defined (default), its values would be proportional to the size of each stratum.
#' @param N A vector of positive integers representing the number of elements in each stratum.
#' @param parameter Type TRUE if you do know the populations SD in sd_exp, or type FALSE (default) if they are estimates.
#'
#' @return This function returns the sample size required to estimate the mean of a variable when using a stratified sampling design without replacement, given the level of risk.
#' @export
#'
#' @details
#' The function to calculate the sample size is:
#' \deqn{n = \frac{\sum_{i=1}^{s} \frac{N_i^2 \cdot \text{sd}_i^2}{\text{alloc}_i} }{ N^2 \cdot \frac{E^2}{N^2 \cdot Z^2} + \sum_{i=1}^{s} N_i \cdot \text{sd}_i^2 }}
#' where 'sd' is parameter 'sd_exp', and 'Z' is the quantile of the two-tailed normal distribution function, compatible with the chosen confidence level 'C'.
#' If 'sd_exp' is unknown, the t-student is used instead of the normal distribution.
#'
#' @examples nt_sts(C = 0.95, E = 200, sd_exp = c(5, 15), alloc = c(1/2, 1/2), N = c(155, 62), parameter = TRUE)
#' @examples nt_sts(C = 0.95, E = 400, sd_exp = c(5, 15, 10), alloc = c(1/2, 1/4, 1/4), N = c(150, 40, 110), parameter = FALSE)


# Sample size function

nt_sts <- function(C, E, sd_exp, alloc = NULL, N, parameter = FALSE) {

  # Default allocation if 'alloc' is not provided
  if (is.null(alloc)) {
    alloc <- N / sum(N)
  }

  # Check parameter ranges
  if (C < 0 || C > 1) {
    stop("Parameter 'C' must be in the range 0 <= C <= 1")
  }

  if (E <= 0) {
    stop("Parameter 'E' must be a positive integer")
  }

  if (!all(sd_exp >= 0)) {
    stop("All elements in 'sd_exp' must be positive integers")
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

  if (length(sd_exp) != length(alloc) || length(sd_exp) != length(N)) {
    stop("'sd_exp', 'alloc', and 'N' must have the same length")
  }

  # Formula to obtain the adjusted sample size
  n <- if (parameter) {
    Z <- qnorm(C + (1 - C) / 2, 0, 1) # qnorm: quantile of the normal distribution
    sum(N^2 * sd_exp^2 / alloc) /
      (sum(N)^2 * E^2 / (Z^2 * sum(N)^2) + sum(N * sd_exp^2))
  } else {
    t <- qt(C + (1 - C) / 2, sum(N) - 1) # qt: quantile of the t-student distribution
    sum(N^2 * sd_exp^2 / alloc) /
      (sum(N)^2 * E^2 / (t^2 * sum(N)^2) + sum(N * sd_exp^2))
  }

  n_adjusted_i <- ceiling(n * alloc)

  return(list(n = sum(n_adjusted_i), n_i = n_adjusted_i))

}
