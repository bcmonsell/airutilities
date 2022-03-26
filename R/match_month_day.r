#' Match a specific month and day
#'
#' Create an indicator variables for a specific month and day.
#'
#' @param x Numeric tis object.
#' @param date_string Character string; Month and day of the date (Example - January 1 is '0101').
#' @return An indicator variable where the week that contains the date entered = 1, 0 otherwise.
#' @examples
#' july4_wed <- match_month_day(ic_week, '0707')
#' @import stats
#' @export
match_month_day <- function(x, date_string) {
    # Author: Brian C. Monsell (OEUS), Version 1.4, 3/23/2021
    
    # generate a vector of strings of dates associated with weekly observations.
    md_string <- substr(as.character(tis::ymd(tis::ti(x))), 5, 8)
    
    # initialize indicator variable with 0
    dummy <- array(0, dim = length(x))
    
    # set observation matching the date to 1
    dummy[md_string == date_string] <- 1
    
    # return indicator variable
    return(dummy)
}
