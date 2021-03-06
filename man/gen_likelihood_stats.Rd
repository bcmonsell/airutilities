% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gen_likelihood_stats.r
\name{gen_likelihood_stats}
\alias{gen_likelihood_stats}
\title{Generate likelihood statistics (AIC, AICC, BIC, etc.)}
\usage{
gen_likelihood_stats(
  Y,
  Lnlkhd,
  Nintvl = 2,
  Np = 2,
  Adj = NULL,
  Logit = FALSE,
  Trans = TRUE,
  Lam = 0,
  Eick = NULL
)
}
\arguments{
\item{Y}{numeric vector, original time series.}

\item{Lnlkhd}{numeric scalar, maximized log likelihood of model.}

\item{Nintvl}{Integer scalar, Number of differences in model used. Default is 2.}

\item{Np}{Integer scalar, Number of parameters in model used. Default is 2.}

\item{Adj}{numeric vector, prior adjustment factor time series. Default is NULL, which indicates there is no prior adjustment.}

\item{Logit}{logical scalar, if TRUE the logit transformation is used. Default is FALSE.}

\item{Trans}{logical scalar, if TRUE a Box-Cox transform is used. Default is TRUE}

\item{Lam}{numeric scalar, Box-Cox transformation parameter. Default is 0 (Log transform).}

\item{Eick}{numeric scalar, weighting paramter for Empiracle Information Criterion. Default is NULL (EIC not computed).}
}
\value{
List of likelihood diagnostics and related estimates (ll = log likelihood, jacadj = jacobian transformation adjustment,
lladj = adjusted likelihood, nobs = number of observations, neffective = effective number of observations,
nparams = number of parameters, df = Nspobs - Nintvl - Np, aic = AIC, aicc = AICC, hannanquinn = Hannan Quinn,
bic = BIC, bic2 = BIC2, EIC = Empiracle Information Criterion)
}
\description{
Generate likelihood statistics that include the Jacobian adjustment for transformations as done in the X-13ARIMA-SEATS program
}
\examples{
ic_default_log_est <- 
   rjd3highfreq::fractionalAirlineEstimation(log(ic_obs), periods=c(365.25/7), 
                                             x=ic_default_matrix)
ic_default_log_lkhd_stat <- 
   gen_likelihood_stats(ic_obs, 
                        Lnlkhd = ic_default_log_est$likelihood$ll, 
                        Nintvl = 2, 
                        Np     = ic_default_log_est$likelihood$nparams)
ic_default_log_aicc <- ic_default_log_lkhd_stat$aicc
}
