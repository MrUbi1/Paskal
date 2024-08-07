% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cp_srs.R
\name{cp_srs}
\alias{cp_srs}
\title{Confidence interval of the proportion under a simple random sampling design}
\usage{
cp_srs(C, n_real, p_est, N = Inf)
}
\arguments{
\item{C}{Level of confidence; 0 <= C <= 1.}

\item{n_real}{Real sample size; n_real > 0.}

\item{p_est}{Sample proportion; 0 <= p_est <= 1.}

\item{N}{A positive integer indicating the number of elements in the population. Defaults to infinite.}
}
\value{
The function returns the confidence interval of the population proportion.
}
\description{
Confidence interval of the proportion under a simple random sampling design
}
\details{
The function to calculate the limit of precision is:
\deqn{LP = Z \cdot \sqrt{\frac{p \cdot (1 - p)}{(n - 1)} \cdot \frac{(N - n)} {N}}}
where 'p' is parameter 'p_est', and 'Z' is the quantile of the two-tailed normal distribution function,
compatible with the chosen confidence level 'C'.
}
\examples{
cp_srs(C = 0.95, n_real = 250, p_est = 0.4)
cp_srs(C = 0.95, n_real = 400, p_est = 0.4)
cp_srs(C = 0.95, n_real = 250, p_est = 0.4, N = 5000)
}
