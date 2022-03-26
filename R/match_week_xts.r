#' Match a specific week
#'
#' Create an indicator variable for a specific week.
#'
#' @param x_week Numeric xts object with week of each observation.
#' @param week_number Numeric scalar; Week of the year to match.
#' @param return_xts Logical scalar; return matrix as an \code{xts} time series object. Default is FALSE.
#' @return An indicator variable where the observation that matches the week entered = 1, 0 otherwise.
#' @examples
#' xmas_w53_xts <- match_week_xts(ic_week_xts, 53)
#' @import stats
#' @export
match_week_xts <- function(x_week, week_number, return_xts = FALSE) {
    # Author: Brian C. Monsell (OEUS), Version 1.3, 3/14/2022
    
    # set up filter with observations that match the number of the week
    week_filter <- x_week == week_number
    
    # initialize indicator variable with 0
    dummy <- array(0, dim = length(x_week))
    
    # set observation matching the filter to 1
    dummy[week_filter] <- 1
    
    # return indicator variable
    if (return_xts) {
       dummy_xts <- 
          xts::xts(x = dummy, 
                   order.by = as.Date(zoo::index(x_week), origin = "1970-01-01"))
       return(dummy_xts)
    } else {
       return(dummy)
    }
}
