#' Plot factors by week for a range of weeks
#'
#' Generate a single plot of seasonal factors by week for a range of weeks
#'
#' @param this_sf Numeric vector of seasonal factors.
#' @param this_week Numeric vector; Week of the year to match.
#' @param first_week Numeric scalar; First week to be included in the plot.
#' @param last_week Numeric scalar; Last week to be included in the plot.
#' @param forecast Numeric scalar; Number of forecasts. Default: 0.
#' @param this_range Numeric scalar; Range of the seasonal factors to be plotted. Default: range taken from the observations being plotted.
#' @param this_trans Logical scalar; set to TRUE if the log transformation is used in the decomposition, FALSE otherwise. Default: TRUE
#' @return Generate a plot of seasonal factors by week. User can specify limits for the weeks plotted in order to generate a multi-panel plot.
#' @examples
#' par(mfrow = c(3, 2), mar = c(3.1, 2.1, 0.5, 0.5), oma = c(0, 0, 3, 0))
#' sf_range <- range(ic_sf)
#' plot_sf_weekly_single(ic_sf, this_week = ic_week,  
#'                       first_week = 1,  last_week = 9,   
#'                       forecast = 0, this_range = sf_range,    
#'                       this_trans = FALSE)
#' plot_sf_weekly_single(ic_sf, this_week = ic_week, 
#'                       first_week = 10, last_week = 18,   
#'                       forecast = 0, this_range = sf_range,    
#'                       this_trans = FALSE)
#' plot_sf_weekly_single(ic_sf, this_week = ic_week, 
#'                       first_week = 19, last_week = 27,   
#'                       forecast = 0, this_range = sf_range,    
#'                       this_trans = FALSE)
#' plot_sf_weekly_single(ic_sf, this_week = ic_week, 
#'                       first_week = 28, last_week = 36,   
#'                       forecast = 0, this_range = sf_range,    
#'                       this_trans = FALSE)
#' plot_sf_weekly_single(ic_sf, this_week = ic_week, 
#'                       first_week = 37, last_week = 45,   
#'                       forecast = 0, this_range = sf_range,    
#'                       this_trans = FALSE)
#' plot_sf_weekly_single(ic_sf, this_week = ic_week, 
#'                       first_week = 46, last_week = 53,   
#'                       forecast = 0, this_range = sf_range,    
#'                       this_trans = FALSE)
#' @import stats
#' @import graphics
#' @export
plot_sf_weekly_single <- function(this_sf, this_week, first_week, last_week, forecast = 0, this_range = NULL, this_trans = TRUE) {
    # Author: Brian C. Monsell (OEUS), Version 2.4, 3/24/2021
    
    # generate number of seasonal factors not including forecasts
    len_sf <- length(as.numeric(this_sf)) - forecast
    
    # put estimated seasonal factors in vector sf
    sf <- this_sf[1:len_sf]
    
    # generate range of estimated seasonal factors
    if (is.null(this_range)) {
        y_limit <- range(sf)
    } else {
        y_limit <- this_range
    }
    
    # generate range of weeks for the x-axis of the plot, number of plots
    x_limit <- seq(first_week - 1, last_week + 1)
    num_plots <- last_week - first_week + 1
    
    # generate assumed mean of seasonal factors
    if (this_trans) {
        h_bar <- 1
    } else {
        h_bar <- 0
    }
    
    # create the plotting frame, with custom x-axis and create reference line for factor mean.
    plot(x_limit, seq(y_limit[1], y_limit[2], length.out = length(x_limit)), type = "n", ylim = y_limit, 
        xlab = " ", ylab = " ", xaxt = "n")
    axis(1, at = first_week:last_week)
    abline(h = h_bar, col = "grey", lty = 2)
    
    # create vector for factor mean.
    this_mu <- array(h_bar, num_plots)
    
    # loop through each week.
    for (i in first_week:last_week) {
        
        # produce limits for week plot.
        s1 <- (i - 1) + 0.6
        s2 <- i + 0.4
        
        this_i <- i - first_week + 1
        
        # save seasonal factors for week i, and generate number of factors.
        this_sf <- sf[this_week == i]
        number_sf <- length(this_sf)
        
        # Generate X values for week i, mean of SF for week i
        this_period_x <- seq(s1, s2, length.out = number_sf)
        this_mu[this_i] <- mean(this_sf)
        
        # Generate line for mean of SF for week i
        segments(s1, this_mu[this_i], s2, this_mu[this_i], col = "green")
        
        # Plot SF for week i
        lines(this_period_x, this_sf, col = "blue")
        
    }
    
    # Join seasonal subplots with grey line
    lines(first_week:last_week, this_mu, col = "grey", lty = 3)
    
}
