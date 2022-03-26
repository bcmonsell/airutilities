#' Generate t-statistics
#'
#' Generate t-statistics from estimates generated from the fractional airline model
#'
#' @param this_beta Numeric vector; estimated model parameters from the fractional airline model.
#' @param this_cov Numeric matrix; estimated covariance matrx from the fractional airline model.
#' @return Numeric vector of t-values
#' @examples
#' ic_air_est <-
#'   rjd3highfreq::fractionalAirlineEstimation(ic_obs, periods=c(365.25/7),
#'                                      x=ic_holiday_matrix,
#'                                      outliers=c('ao', 'ls'),
#'                                      criticalValue = 6)
#' ic_air_t_reg   <- t_values_air(ic_air_est$model$b, ic_air_est$model$bcov)
#' ic_air_t_arima <- t_values_air(ic_air_est$estimation$parameters,
#'                                ic_air_est$estimation$covariance)
#' @import stats
#' @export
t_values_air <- function(this_beta, this_cov) {
    # Author: Brian C. Monsell (OEUS), Version 2.0, 2/2/2022
    
    # generate length of the beta vector, initialize t-statistics vector to zero
    this_n <- length(this_beta)
    this_tvalue <- array(0, dim = 0)
    
    # generate t-statistics
    for (i in 1:this_n) {
        this_tvalue[i] <- this_beta[i]/sqrt(this_cov[i, i])
    }
    
    # return t-statistics
    return(this_tvalue)
}
