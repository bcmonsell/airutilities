#' Generate temporary change outlier regression matrix
#'
#' Generates a regression matrix of TC (temporary change) outliers to be included in weekly modeling routines
#'
#' @param outlier_dates Integer matrix - matrix of dates for TC outliers
#' @param this_week Numeric vector; Week of the year for each observation.
#' @param this_year Numeric vector; Year of each observation.
#' @param forecast Numeric scalar; Number of forecasts. Default: 0.
#' @param air_name Logical scalar; If TRUE, names are formatted as they are in the fractional airline routine; 
#'                 otherwise format them as in movereg. Default: TRUE
#' @param this_freq Numeric scalar; frequency of time series. Default: 365.25/7, for a weekly series
#' @param tc_alpha Numeric scalar; Rate of decay for the TC outlier. 
#'                 Default: will be computed as in \code{X-13ARIMA-SEATS} for a weekly series
#' @param return_xts Logical scalar; return matrix as an \code{xts} time series object. Default is FALSE.
#'        If TRUE, \code{this_week} and \code{this_year} should be \code{xts} time series objects.
#' @return Generate matrix of temporary change outlier regressors, with column names that describe the individual regressors
#' @examples
#' ic_tc_dates <-
#'    matrix(c(13, 2020), ncol=2, byrow=TRUE)
#' ic_tc_matrix <-
#'    gen_level_outlier_matrix(ic_tc_dates, ic_week, ic_year, 0,
#'                             return_xts = FALSE)
#' cc_tc_dates <-
#'    matrix(c(13, 2020), ncol=2, byrow=TRUE)
#' cc_tc_matrix_xts <-
#'    gen_level_outlier_matrix(cc_tc_dates, cc_week_xts, cc_year_xts, 0,
#'                             return_xts = TRUE)
#' @import stats
#' @export
gen_tc_outlier_matrix <- function(outlier_dates, this_week, this_year, forecast = 0, air_name = TRUE, this_freq = NULL, 
                                  tc_alpha = NULL, return_xts = FALSE) {
    # Author: Brian C. Monsell (OEUS), Version 2.2, 3/14/2022
    
    # Set number of outliers, length of outlier regressors
    num_otlr <- nrow(outlier_dates)
    len_otlr <- length(this_week) + forecast
    
    # If \code{tc_alpha} is not set, compute as in \code{X-13ARIMA-SEATS}
    if (is.null(this_freq)) {
        this_freq <- 365.25 / 7
    }
    if (is.null(tc_alpha)) {
        tc_alpha <- 0.7^(12/this_freq)
    }
    
    # Inititalize outlier matrix to 0
    outlier_matrix <- matrix(0, ncol = num_otlr, nrow = len_otlr)
    
    # for each outlier, create a filter indicating which observation matches the outlier date and set
    # that observation to one in column i of the outlier matrix
    for (i in 1:num_otlr) {
        this_filter <- (this_week == outlier_dates[i, 1]) & (this_year == outlier_dates[i, 2])
        
        # Inititalize \code{this_obs} and \code{this_pos}
        this_obs <- 0
        this_pos <- len_otlr + 1
        
        for (j in 1:len_otlr) {
            # Update \code{this_obs} and \code{this_pos} at date of TC outlier for observation j
            if (this_filter[j]) {
                this_obs <- 1
                this_pos <- j
            }
            
            # Set observation j based on whether it is before or after the outlier date
            if (j > this_pos) {
                outlier_matrix[j, i] <- outlier_matrix[j - 1, i] * tc_alpha
            } else {
                outlier_matrix[j, i] <- this_obs
            }
        }
    }
    
    # Generate outlier names, set column names of outlier matrix to outlier names
    if (air_name) {
        outlier_names <- paste0("TC(week ", outlier_dates[, 1], ", ", outlier_dates[, 2], ")")
    } else {
        outlier_names <- paste("TC week", outlier_dates[, 1], outlier_dates[, 2])
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
