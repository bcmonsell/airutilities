#' Plot of factor means
#'
#' Generates a plot of the means of factors
#'
#' @param this_factor \code{tis} object of the factors from a weekly seasonal adjustment
#' @param this_period Integer scalar; vector with period number of the observations.
#' @param this_col Character scalar; color used for factor plots. Default is \code{green}.
#' @param y_limit Numeric vector of length 2; Range of values you wish the plot to be plotted over. Default is range of the seasonal factors.
#' @param this_freq integer scalar; time series frequency.
#' @param this_trans Logical scalar; indicates if the adjustment was done with a log transform. Default is TRUE.
#' @param this_title Character string; main title of plot.  Default is \code{'Mean of Seasonal Factors'}.
#' @param forecast Integer scalar; Number of forecasts appended to the factors. Default is 0.
#' @param this_type Character string; type of factors plotted.  Default is  \code{'seasonal'}.
#' @param add_line Logical scalar; indicates if this line is being added to an existing plot. Default is FALSE.
#' @return Generate plot of the means of factors by period, or add a line to an existing plot. If factors not specified, print out error message and return NULL.
#' @examples
#' ic_sf_range <- range(ic_sf, ic_sf_auto, ic_sf_auto_ljung, ic_sf_tc)
#' this_color <- c("#4682B4", "#7846B4", "#B47846", "#82B446")
#' plot_sf_mean(ic_sf, 
#'              this_period = ic_week, 
#'              this_col = this_color[1], 
#'              y_limit = ic_sf_range,
#'              this_freq = 53, 
#'              this_type = 'Combined', 
#'              this_trans = FALSE,
#'              this_title = 'Initial Claims (Weekly Combined Factors)')
#' plot_sf_mean(ic_sf_auto, 
#'              this_period = ic_week, 
#'              this_col = this_color[2], 
#'              this_freq = 53, 
#'              this_trans = FALSE,
#'              add_line = TRUE)
#' plot_sf_mean(ic_sf_auto_ljung, 
#'              this_period = ic_week, 
#'              this_col = this_color[3], 
#'              this_freq = 53, 
#'              this_trans = FALSE,
#'              add_line = TRUE)
#' plot_sf_mean(ic_sf_tc, 
#'              this_period = ic_week, 
#'              this_col = this_color[4], 
#'              this_freq = 53, 
#'              this_trans = FALSE,
#'              add_line = TRUE)
#' legend('topright', 
#'        legend=c('ic (default)', 'ic (auto outliers)', 'ic (auto, ljung cv)', 'ic (auto tc)'),
#'        col=this_color, 
#'        lty=rep(1,4), 
#'        cex=0.75)
#' @import stats
#' @import graphics
#' @export
plot_sf_mean <- function(this_factor = NULL, this_period = NULL, this_col = "green", y_limit = range(this_factor),
    this_freq, this_trans = TRUE, this_title = "Mean of Seasonal Factors", forecast = 0, this_type = "Seasonal",
    add_line = FALSE) {
    # Author: Brian C. Monsell (OEUS), Version 1.10, 3/23/2021

    # Check if factors, period is specified
    if (is.null(this_factor)) {
        stop("Argument this_factor must be specified.")
    }

    if (is.null(this_period)) {
        stop("Argument this_period must be specified.")
    }

    # Extract seasonal factors
    sf <- this_factor[1:(length(this_factor) - forecast)]

    if (length(this_period) > length(sf)) {
        period <- this_period[1:(length(this_period) - forecast)]
    } else {
        period <- this_period
    }

    # Initialize vector of seasonal factor means
    this_factor_mean <- array(0, this_freq)

    # Compute seasonal means
    for (i in 1:this_freq) {
        this_factor_mean[i] <- mean(sf[period == i])
    }

    # Add line for seasonal means to plot
    if (add_line) {
        lines(1:this_freq, this_factor_mean, type = "b", col = this_col)
    } else {
        # set value for horizontal line
        if (this_trans) {
            h_bar <- 1
        } else {
            h_bar <- 0
        }
        # Plot seasonal means
        plot(this_factor_mean, type = "b", main = this_title, sub = paste("Average ", this_type, " Factors by Week of Year"),
            ylab = " ", xlab = " ", ylim = y_limit, col = this_col)
        # generate horizontal line
        abline(h = h_bar, col = "grey", lty = 2)
    }
}
