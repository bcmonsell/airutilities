#' Generate point outlier regression matrix
#'
#' Generates a regression matrix of AO (point) outliers to be included in weekly modeling routines
#'
#' @param outlier_dates Integer matrix - matrix of dates for point outliers
#' @param this_week Numeric vector; Week of the year for each observation.
#' @param this_year Numeric vector; Year of each observation.
#' @param forecast Numeric scalar; Number of forecasts. Default: 0.
#' @param air_name Logical scalar; If TRUE, names are formatted as they are in the fractional airline routine; 
#'                 otherwise format them as in movereg. Default: TRUE
#' @param return_xts Logical scalar; return matrix as an \code{xts} time series object. Default is FALSE.
#'        If TRUE, \code{this_week} and \code{this_year} should be \code{xts} time series objects.
#' @return Generate matrix of point outlier regressors, with column names that describe the individual regressors
#' @examples
#' ic_outlier_date <-
#'    matrix(c(30, 1992, 30, 1993, 52, 1993,  5, 1994,  3, 1996,
#'             38, 2001, 39, 2001, 40, 2001, 41, 2001, 42, 2001,
#'             43, 2001, 47, 2001, 48, 2001, 37, 2005, 38, 2005,
#'             39, 2005, 40, 2005, 41, 2005,  1, 2006,  2, 2007,
#'              4, 2008, 45, 2012, 35, 2017, 12, 2020, 13, 2020,
#'             14, 2020, 15, 2020, 16, 2020), ncol=2, byrow=TRUE)
#' ic_outlier_matrix_1992 <-
#'    gen_outlier_matrix(ic_outlier_date, ic_week, ic_year, 0,
#'                       return_xts = FALSE)
#' cc_outlier_dates_1992 <-
#'    matrix(c(30, 1992, 30, 1993, 52, 1993,  5, 1994,  3, 1996,
#'             38, 2001, 39, 2001, 40, 2001, 41, 2001, 42, 2001,
#'             43, 2001, 47, 2001, 48, 2001, 37, 2005, 38, 2005,
#'             39, 2005, 40, 2005, 41, 2005,  1, 2006,  2, 2007,
#'              4, 2008, 45, 2012, 35, 2017, 12, 2020, 13, 2020), ncol=2, byrow=TRUE)
#' cc_level_matrix_xts_1992 <-
#'    gen_level_outlier_matrix(cc_outlier_dates_1992, cc_week_xts, cc_year_xts, 0,
#'                             return_xts = TRUE)
#' @import stats
#' @export
gen_outlier_matrix <- function(outlier_dates, this_week, this_year, forecast = 0, air_name = TRUE,
                               return_xts = FALSE) {
    # Author: Brian C. Monsell (OEUS), Version 2.2, 3/14/2022
    
    # Set number of outliers, length of outlier regressors
    num_otlr <- nrow(outlier_dates)
    len_otlr <- length(this_week) + forecast
    
    # Inititalize outlier matrix to 0
    outlier_matrix <- matrix(0, ncol = num_otlr, nrow = len_otlr)
    
    # for each outlier, create a filter indicating which observation matches the outlier date and set
    # that observation to one in column i of the outlier matrix
    for (i in 1:num_otlr) {
        this_filter <- (this_week == outlier_dates[i, 1]) & (this_year == outlier_dates[i, 2])
        outlier_matrix[this_filter, i] <- 1
    }
    
    # Generate outlier names, set column names of outlier matrix to outlier names
    if (air_name) {
        outlier_names <- paste0("AO(week ", outlier_dates[, 1], ", ", outlier_dates[, 2], ")")
    } else {
        outlier_names <- paste("AO week", outlier_dates[, 1], outlier_dates[, 2])
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
