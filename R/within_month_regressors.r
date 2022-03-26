#' Generate within-month effect regression matrix
#'
#' Generates a regression matrix of within month effects from Cleveland and Scott to be included in weekly modeling routines
#'
#' @param this_year Numeric vector; Year of each observation.
#' @param this_month Numeric vector; Month for each observation.
#' @param this_day Numeric vector; Day of the month for each observation.
#' @param l Numeric scalar; Number of sin-cos terms to generate, with the total number of regressors being \code{2*l}. Default: 30. 
#' @param return_xts Logical scalar; return matrix as an \code{xts} time series object. Default is FALSE.
#' @return Generate matrix of within month effects regressors, with column names that describe the individual regressors
#' @examples
#' ic_wm_matrix_1992 <-
#'    within_month_regressors(ic_year, ic_month, ic_day, return_xts = FALSE)
#' cc_wm_matrix_xts <-
#'    within_month_regressors(cc_year_xts, cc_month_xts, cc_day_xts, 
#'                            return_xts = TRUE)
#' @import stats
#' @export
within_month_regressors <- function(this_year, this_month, this_day, l=30, return_xts = TRUE) {
    # Author: Brian C. Monsell (OEUS), Version 2.2, 3/2/2022
   this_length <- length(this_year)
   this_matrix <- matrix(rep(0.0, 2*l*this_length), ncol = 2*l)
   
   this_col_name <- array(" ", dim = 2*l)

   this_date <- as.Date(paste0(this_year, "-", this_month, "-", this_day),
                        '%Y-%m-%d')
   this_lom  <- lubridate::days_in_month(this_date)
   
   i2 <- 0
   for (i in 1:l) {
      this_sin <- sin(2 * pi * i * this_day / this_lom)
      this_cos <- cos(2 * pi * i * this_day / this_lom)
      
      i2 <- i2 + 1
      this_matrix[,i2] <- this_sin
      this_col_name[i2] <- paste0("sin", i)
      
      i2 <- i2 + 1
      this_matrix[,i2] <- this_cos
      this_col_name[i2] <- paste0("cos", i)
   }
   
   colnames(this_matrix) <- this_col_name
   
    if (return_xts) {
       this_matrix_xts <- xts::xts(x = this_matrix, order.by = this_date)
       return(this_matrix_xts)
    } else {
       return(this_matrix)
    }

}