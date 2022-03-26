#' Generate level change regression matrix
#'
#' Generates a regression matrix of LS (level change) outliers to be included in weekly modeling routines
#'
#' @param outlier_dates Integer matrix - matrix of dates for LS outliers
#' @param this_week Numeric vector; Week of the year for each observation.
#' @param this_year Numeric vector; Year of each observation.
#' @param forecast Numeric scalar; Number of forecasts. Default: 0.
#' @param air_name Logical scalar; If TRUE, names are formatted as they are in the fractional airline routine; 
#'                 otherwise format them as in movereg. Default: TRUE
#' @param x13type Logical scalar; Indicates if level change outlier is defined as in X-13ARIMA-SEATS. Default: TRUE. 
#' @param return_xts Logical scalar; return matrix as an \code{xts} time series object. Default is FALSE.
#'        If TRUE, \code{this_week} and \code{this_year} should be \code{xts} time series objects.
#' @return Generate matrix of level change outlier regressors, with column names that describe the individual regressors
#' @examples
#' ic_level_dates <-
#'    matrix(c(12, 2020, 13, 2020), ncol=2, byrow=TRUE)
#' ic_level_matrix <-
#'    gen_level_outlier_matrix(ic_level_dates, ic_week, ic_year, 0,
#'                             return_xts = FALSE)
#' cc_level_dates <-
#'    matrix(c(12, 2020, 13, 2020), ncol=2, byrow=TRUE)
#' cc_level_matrix_xts <-
#'    gen_level_outlier_matrix(cc_level_dates, cc_week_xts, cc_year_xts, 0,
#'                             return_xts = TRUE)
#' @import stats
#' @export
gen_level_outlier_matrix <- function(outlier_dates, this_week, this_year, forecast = 0, air_name = TRUE, x13type = TRUE,
                                     return_xts = FALSE) {
    # Author: Brian C. Monsell (OEUS), Version 4.2, 3/14/2021
    
    # Set number of outliers, length of outlier regressors
    num_otlr <- nrow(outlier_dates)
    len_otlr <- length(this_week) + forecast
    
    # Inititalize outlier matrix to 0
    outlier_matrix <- matrix(0, ncol = num_otlr, nrow = len_otlr)
    
    
    # Format outlier regressor to maintain the level of the series for the most recent observations
    if (x13type) {
        for (i in 1:num_otlr) {
            # set constant used to set the current observation to -1
            this_obs <- -1
            
            # generate a logical filter to match the outlier date for outlier i
            this_filter <- (this_week == outlier_dates[i, 1]) & (this_year == outlier_dates[i, 2])
            
            for (j in 1:len_otlr) {
                # change constant to 0 when filter matches current observation
                if (this_filter[j]) {
                  this_obs <- 0
                }
                outlier_matrix[j, i] <- this_obs
            }
        }
    } else {
        for (i in 1:num_otlr) {
            # set constant used to set the current observation to 0
            this_obs <- 0

            # generate a logical filter to match the outlier date for outlier i
            this_filter <- (this_week == outlier_dates[i, 1]) & (this_year == outlier_dates[i, 2])
            
            for (j in 1:len_otlr) {
                # change constant to 1 when filter matches current observation
                if (this_filter[j]) {
                  this_obs <- 1
                }
                outlier_matrix[j, i] <- this_obs
            }
        }
    }
    
    # Generate outlier names, set column names of outlier matrix to outlier names
    if (air_name) {
        outlier_names <- paste0("LS(week ", outlier_dates[, 1], ", ", outlier_dates[, 2], ")")
    } else {
        outlier_names <- paste("LS week", outlier_dates[, 1], outlier_dates[, 2])
    }
    colnames(outlier_matrix) <- outlier_names
    
    # return outlier matrix
    if (return_xts) {
       outlier_matrix_xts <- 
          xts::xts(x = outlier_matrix, 
                   order.by = as.Date(zoo::index(this_week), origin = "1970-01-01"))
       return(outlier_matrix_xts)
    } else {
       return(outlier_matrix)
    }

}
