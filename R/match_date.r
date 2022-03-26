#' Match a specific date
#'
#' Create an indicator variables for a specific date (year, month, day) for a weekly time series as defined in \code{tis}.
#'
#' @param x Numeric tis object.
#' @param date_string Character string; Year, month and day of the date (Example - January 1, 1990 is '19900101').
#' @return An indicator variable where the week contains the date entered = 1, 0 otherwise.
#' @examples
#' pandemic_start <- match_date(ic_week, '20200317')
#' @import stats
#' @export
match_date <- function(x, date_string) {
    # Author: Brian C. Monsell (OEUS), Version 1.4, 3/23/2021
    
    # generate a vector of strings of dates associated with weekly observations.
    ymd_string <- as.character(tis::ymd(tis::ti(x)))
    
    # initialize indicator variable with 0
    dummy <- array(0, dim = length(x))
    
    # set observation matching the date to 1
    dummy[ymd_string == date_string] <- 1
    
    # return indicator variable
    return(dummy)
}
