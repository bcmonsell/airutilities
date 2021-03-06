% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gen_air_model_matrix.r
\name{gen_air_model_matrix}
\alias{gen_air_model_matrix}
\title{Fractional airline model summary}
\usage{
gen_air_model_matrix(
  this_est,
  xreg_names = NULL,
  this_week = NULL,
  this_year = NULL
)
}
\arguments{
\item{this_est}{A list object generated by the estimation procedure of the fractional airline model}

\item{xreg_names}{Character vector; Names of the user-defined regression variables.}

\item{this_week}{Numeric vector; Week of each observation. Default: NULL}

\item{this_year}{Numeric vector; Year of each observation. Default: NULL}
}
\value{
Generate matrix of model parameters estimated for the fractional airline model
}
\description{
Generates a matrix with coefficient estimates from the fractional airline model, generated by the JDemtetra+ high dimension modeling R routines
}
\examples{
ic_default_est <-
   rjd3highfreq::fractionalAirlineEstimation(ic_obs, periods=c(365.25/7), x=ic_default_matrix)
ic_default_model <- 
   gen_air_model_matrix(ic_default_est,
                        xreg_names = colnames(ic_default_matrix),
                        this_week = ic_week, this_year = ic_year)
}
