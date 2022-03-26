#' Generate weekly boxplots
#'
#' Generate a multi-panel plot of boxplots by week
#'
#' @param this_factor Numeric vector of factors from a weekly seasonal adjustment.
#' @param this_week Numeric vector; Week of the year to match.
#' @param forecast Numeric scalar; Number of forecasts. Default: 0.
#' @param this_range Numeric scalar; Range of the seasonal factors to be plotted. Default: range taken from the observations being plotted.
#' @param this_type Character string; type of factor being plotted. Default: 'Irregular'
#' @param this_title Character string; Main title for plot.
#' @return Generate a mulit-panel plot of boxplots of factors by week. Need to have \code{plot_boxplot_single} loaded to run.
#' @examples
#' ic_sf_range <- 
#'    range(ic_sf, ic_sf_auto)
#' plot_boxplot_weekly(ic_sf, ic_week, 
#'                     forecast = 0,
#'                     this_range = ic_sf_range,
#'                     this_type  = "Seasonal",
#'                     this_title = 'Initial Claims (default)')
#' plot_boxplot_weekly(ic_sf_auto, ic_week, 
#'                     forecast = 0,
#'                     this_range = ic_sf_range, 
#'                     this_type  = "Seasonal",
#'                     this_title = 'Initial Claims (auto outliers)')
#' @import stats
#' @import graphics
#' @export
plot_boxplot_weekly <- function(this_factor, this_week, forecast = 0, this_range = NULL, this_type = "Irregular", this_title) {
    # Author: Brian C. Monsell (OEUS), Version 2.4, 1/12/2022
    
    # save current graphics parameters, set graphics parameters to set up 6 panel plot
    old_par <- par(no.readonly = T)
    par(mfrow = c(3, 2), mar = c(3.1, 2.1, 0.5, 0.5), oma = c(0, 0, 3, 0))
    
    # plot each panel, with about 9 weeks in each panel
    for (i in seq(1,46,9)) {
        i2 <- i + 8
        if (i2 > 53) { 
           i2 <- 53 
        }
        plot_boxplot_single(this_factor, this_week, i, i2, forecast = forecast, 
                            this_range = this_range)
    }

    # generate main and sub title for plot
    mtext(this_title, 3, 1.25, outer = TRUE)
    mtext(paste0(this_type, " Boxplots"), 3, 0.25, outer = TRUE, cex = 0.75)
    
    # restore saved graphics parameters
    par(old_par)
    
}
