#' Generate Movreg holiday regression matrix
#'
#' Generates a matrix with holiday regressors as defined within the BLS weekly adjustment program Movereg
#'
#' @param hol_n Numeric scalar; number of weights for this particular holiday
#' @param hol_index Numeric scalar; position of holiday within the weight vector
#' @param hol_wt Numeric vector; weight vector for holiday effect
#' @param hol_type Character scalar; Type of holiday (\code{'newyear'} for New Years Day, \code{'mlk'} for MLK Observance, \code{'president'} for President's Day, \code{'easter'} for Easter, \code{'memorial'} for Memorial Day, \code{'july4'} for Independence Day, \code{'labor'} for Labor Day, \code{'columbus'} for Columbus Day, \code{'veteran'} for Veterans Day, \code{'thanks'} for Thanksgiving).
#' @param this_week Numeric vector; Week of the year for each observation.
#' @param this_year Numeric vector; Year of each observation.
#' @return Generate matrix of holiday regressors as defined within the BLS weekly adjustment program Movereg
#' @examples
#' ic_easter  <-
#'   gen_movereg_holiday(hol_n = 8,
#'                       hol_index = 8,
#'                       hol_wt = c(1, 0, 0, 0, 0, 0, 0, 0),
#'                       hol_type = 'easter',
#'                       this_week = ic_week,
#'                       this_year = ic_year)
#' ic_labor  <-
#'   gen_movereg_holiday(hol_n = 2,
#'                       hol_index = 2,
#'                       hol_wt = c(0, 1),
#'                       hol_type = 'labor',
#'                       this_week = ic_week,
#'                       this_year = ic_year)
#' ic_thanksgiving <-
#'   gen_movereg_holiday(hol_n = 1,
#'                       hol_index = 1,
#'                       hol_wt = array(1, dim=1),
#'                       hol_type = 'thanksgiving',
#'                       this_week = ic_week,
#'                       this_year = ic_year)
#' ic_holiday_matrix_reduced <- cbind(ic_easter, ic_labor, ic_thanksgiving)
#' colnames(ic_holiday_matrix_reduced) <- c('easter', 'labor', 'thanksgiving')
#' @import stats
#' @export
gen_movereg_holiday <- function(hol_n, hol_index, hol_wt, hol_type, this_week, this_year) {
    # Author: Brian C. Monsell (OEUS), Version 2.10, 3/23/2021
    
    # Set first and last week of the series
    first_week <- this_week[1]
    last_week <- this_week[length(this_week)]
    
    # Set first and last year of the series
    first_year <- min(this_year)
    last_year <- max(this_year)
    
    # Initialize \code{this_week_range}
    this_week_range <- NULL
    
    # Generate holiday dates for all the holiday starts General procedure is to set a week range,
    # adjust the first and last year the holiday is defined for, generate the year sequence, set the
    # holiday dates based on the year sequence
    if (hol_type == "newyear") {
        this_week_range <- 1:2
        if (first_week > 1) {
            first_year <- first_year + 1
        }
        if (last_week < 2) {
            last_year <- last_year - 1
        }
        this_year_seq <- seq(first_year, last_year)
        this_hol_dates <- as.Date(timeDate::NewYearsDay(this_year_seq))
    }
    if (hol_type == "mlk") {
        this_week_range <- 2:4
        if (first_week > 2) {
            first_year <- first_year + 1
        }
        if (last_week < 4) {
            last_year <- last_year - 1
        }
        this_year_seq <- seq(first_year, last_year)
        this_hol_dates <- as.Date(timeDate::USMLKingsBirthday(this_year_seq))
    }
    if (hol_type == "president") {
        this_week_range <- 7:9
        if (first_week > 7) {
            first_year <- first_year + 1
        }
        if (last_week < 9) {
            last_year <- last_year - 1
        }
        this_year_seq <- seq(first_year, last_year)
        this_hol_dates <- as.Date(timeDate::USPresidentsDay(this_year_seq))
    }
    if (hol_type == "easter") {
        this_week_range <- 12:18
        if (first_week > 12) {
            first_year <- first_year + 1
        }
        if (last_week < 18) {
            last_year <- last_year - 1
        }
        this_year_seq <- seq(first_year, last_year)
        this_hol_dates <- as.Date(timeDate::Easter(this_year_seq))
    }
    if (hol_type == "memorial") {
        this_week_range <- 22:23
        if (first_week > 22) {
            first_year <- first_year + 1
        }
        if (last_week < 25) {
            last_year <- last_year - 1
        }
        this_year_seq <- seq(first_year, last_year)
        this_hol_dates <- as.Date(timeDate::USMemorialDay(this_year_seq))
    }
    if (hol_type == "july4") {
        this_week_range <- 25:29
        if (first_week > 25) {
            first_year <- first_year + 1
        }
        if (last_week < 29) {
            last_year <- last_year - 1
        }
        this_year_seq <- seq(first_year, last_year)
        this_hol_dates <- as.Date(timeDate::USIndependenceDay(this_year_seq))
    }
    if (hol_type == "labor") {
        this_week_range <- 35:37
        if (first_week > 35) {
            first_year <- first_year + 1
        }
        if (last_week < 37) {
            last_year <- last_year - 1
        }
        this_year_seq <- seq(first_year, last_year)
        this_hol_dates <- as.Date(timeDate::USLaborDay(this_year_seq))
    }
    if (hol_type == "columbus") {
        this_week_range <- 41:42
        if (first_week > 41) {
            first_year <- first_year + 1
        }
        if (last_week < 42) {
            last_year <- last_year - 1
        }
        this_year_seq <- seq(first_year, last_year)
        this_hol_dates <- as.Date(timeDate::USColumbusDay(this_year_seq))
    }
    if (hol_type == "veteran") {
        this_week_range <- 44:46
        if (first_week > 44) {
            first_year <- first_year + 1
        }
        if (last_week < 46) {
            last_year <- last_year - 1
        }
        this_year_seq <- seq(first_year, last_year)
        this_hol_dates <- as.Date(timeDate::USVeteransDay(this_year_seq))
    }
    if (hol_type == "thanksgiving") {
        this_week_range <- 46:48
        if (first_week > 46) {
            first_year <- first_year + 1
        }
        if (last_week < 48) {
            last_year <- last_year - 1
        }
        this_year_seq <- seq(first_year, last_year)
        this_hol_dates <- as.Date(timeDate::USThanksgivingDay(this_year_seq))
    }
    
    # if week range not set, generate error message as holiday type not defined
    if (is.null(this_week_range)) {
        stop("ERROR: Holiday type not found. Choices limited to newyears, mlk, president, easter, memorial, july4, labor, columbus, veteran, and thanksgiving.")
    }
    
    # if initialize holiday regressor, index vector
    this_hol_reg <- array(0, dim = length(this_week))
    this_index <- seq(1, length(this_week))
    
    # generate regressor for each year
    for (i in 1:length(this_year_seq)) {
        # Set date of start of holiday window
        this_day_start <- this_hol_dates[i] - hol_index + 1
        # initialize index for week range variable
        j <- 0
        # Go through range of weeks till the week the holiday window starts
        repeat {
            j <- j + 1
            this_filter <- (this_year == this_year_seq[i]) & (this_week == this_week_range[j])
            first_week_index <- this_index[this_filter]
            this_week_date <- as.Date(tis::ti(this_week)[first_week_index])
            this_date_diff <- as.numeric(this_week_date - this_day_start)
            if (this_date_diff > -1) {
                break
            }
        }
        # Set position of end of holiday window for this week
        k1 <- min(this_date_diff + 1, hol_n)
        # process weight vector to generate holiday regressor for this observation
        for (k in 1:k1) {
            this_hol_reg[first_week_index] <- this_hol_reg[first_week_index] + hol_wt[k]
        }
        # determine if holiday window continues into the next week
        if (hol_n > k1) {
            # update index
            first_week_index <- first_week_index + 1
            # process weight vector to generate holiday regressor for this observation
            for (k in seq(k1 + 1, hol_n)) {
                this_hol_reg[first_week_index] <- this_hol_reg[first_week_index] + hol_wt[k]
            }
        }
    }
    
    # return holiday regressor
    return(this_hol_reg)
    
}
