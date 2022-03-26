#' Match a specific month and day
#'
#' Create an indicator variables for a specific month and day.
#'
#' @param x Numeric xts object.
#' @param date_string Character string; Month and day of the date (Example - January 1 is '0101').
#' @param return_xts Logical scalar; return matrix as an \code{xts} time series object. Default is FALSE.
#' @return An indicator variable where the week that contains the date entered = 1, 0 otherwise.
#' @examples
#' july4_wed_xts <- match_month_day_xts(ic_week_xts, '0707')
#' @import stats
#' @export
match_month_day_xts <- function(x, date_string, return_xts = FALSE) {
    # Author: Brian C. Monsell (OEUS), Version 1.3, 3/14/2022
    
    # generate a vector of strings of dates associated with weekly observations.
    md_string <- gsub("-", "", substr(as.character(zoo::index(x)),6,11))
    
    # initialize indicator variable with 0
    dummy <- array(0, dim = length(x))
    
    # set observation matching the date to 1
    dummy[md_string == date_string] <- 1
    
    # return indicator variable
    if (return_xts) {
       dummy_xts <- 
          xts::xts(x = dummy, 
                   order.by = as.Date(zoo::index(x), origin = "1970-01-01"))
       return(dummy_xts)
    } else {
       return(dummy)
    }
}
