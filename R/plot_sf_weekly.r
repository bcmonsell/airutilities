#' Plot factors by week
#'
#' Generate a multi-panel plot of seasonal factors by week
#'
#' @param this_sf Numeric vector of seasonal factors.
#' @param this_week Numeric vector; Week of the year to match.
#' @param forecast Numeric scalar; Number of forecasts. Default: 0.
#' @param this_range Numeric scalar; Range of the seasonal factors to be plotted. Default: range taken from the observations being plotted.
#' @param this_trans Logical scalar; set to TRUE if the log transformation is used in the decomposition, FALSE otherwise. Default: TRUE
#' @param this_type Character string; type of factor being plotted. Default: 'Seasonal'
#' @param this_title Character string; Main title for plot.
#' @return Generate a mulit-panel plot of seasonal factors by week. Need to have \code{plot_sf_weekly_single} loaded to run.
#' @examples
#' ic_sf_range <- range(ic_sf, ic_sf_auto, ic_sf_auto_ljung, ic_sf_tc)
#' plot_sf_weekly(ic_sf, 
#'                this_week = ic_week,  
#'                forecast = 0,
#'                this_range = ic_sf_range,  
#'                this_trans = FALSE,  
#'                this_type = 'Combined',
#'                this_title = 'Initial Claims (default)')
#' plot_sf_weekly(ic_sf_auto,  
#'                this_week = ic_week,  
#'                forecast=0,
#'                this_range = ic_sf_range,  
#'                this_trans = FALSE,  
#'                this_type = 'Combined',
#'                this_title = 'Initial Claims (auto)')
#' plot_sf_weekly(ic_sf_auto_ljung,  
#'                this_week = ic_week,  
#'                forecast=0,
#'                this_range = ic_sf_range,  
#'                this_trans = FALSE,  
#'                this_type = 'Combined',
#'                this_title = 'Initial Claims (auto, ljung cv)')
#' plot_sf_weekly(ic_sf_tc,  
#'                this_week = ic_week,  
#'                forecast=0,
#'                this_range = ic_sf_range,  
#'                this_trans = FALSE,  
#'                this_type = 'Combined',
#'                this_title = 'Initial Claims (fractional airline)')
#' @import stats
#' @import graphics
#' @export
plot_sf_weekly <- function(this_sf, this_week, forecast = 0, this_trans = TRUE, this_range = NULL, this_type = "Seasonal", 
    this_title) {
    # Author: Brian C. Monsell (OEUS), Version 2.7, 3/24/2021
    
    # save current graphics parameters, set graphics parameters to set up 6 panel plot
    old_par <- par(no.readonly = T)
    par(mfrow = c(3, 2), mar = c(3.1, 2.1, 0.5, 0.5), oma = c(0, 0, 3, 0))
    
    # plot each panel, with about 8 weeks in each panel
    for (i in seq(1,46,9)) {
        i2 <- i + 8
        if (i2 > 53) { 
           i2 <- 53 
        }
        plot_sf_weekly_single(this_sf, this_week, i, i2, forecast = forecast, 
                              this_trans = this_trans, this_range = this_range)
    }

    # generate main and sub title for plot
    mtext(this_title, 3, 1.25, outer = TRUE)
    mtext(paste(this_type, " Factor Sub-Plots"), 3, 0.25, outer = TRUE, cex = 0.75)
    
    # restore saved graphics parameters
    par(old_par)
    
}
