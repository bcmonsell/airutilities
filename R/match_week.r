#' Match a specific week
#'
#' Create an indicator variable for a specific week.
#'
#' @param x_week Numeric tis object.
#' @param week_number Numeric scalar; Week of the year to match.
#' @return An indicator variable where the observation that matches the week entered = 1, 0 otherwise.
#' @examples
#' xmas_w53 <- match_week(ic_week, 53)
#' @import stats
#' @export
match_week <- function(x_week, week_number) {
    # Author: Brian C. Monsell (OEUS), Version 1.2, 3/23/2021
    
    # generate a vector of strings of dates associated with weekly observations.
    date_index <- tis::ymd(tis::ti(x_week))
    
    # set up filter with observations that match the number of the week
    week_filter <- date_index[x_week == week_number]
    
    # initialize indicator variable with 0
    dummy <- array(0, dim = length(x_week))
    
    # set observation matching the filter to 1
    dummy[date_index %in% week_filter] <- 1
    
    # return indicator variable
    return(dummy)
}
