#' Generate one set of weekly boxplots
#'
#' Generate a single set of boxplots of factors by week for a range of weeks
#'
#' @param this_factor Numeric vector of factors from weekly seasonal adjustment.
#' @param this_week Numeric vector; Week of the year to match.
#' @param first_week Numeric scalar; First week to be included in the plot.
#' @param last_week Numeric scalar; Last week to be included in the plot.
#' @param forecast Numeric scalar; Number of forecasts. Default: 0.
#' @param this_range Numeric scalar; Range of the seasonal factors to be plotted. Default: range taken from the observations being plotted.
#' @return Generate a plot of seasonal factors by week. User can specify limits for the weeks plotted in order to generate a multi-panel plot.
#' @examples
#' par(mfrow = c(3, 2), mar = c(3.1, 2.1, 0.5, 0.5), oma = c(0, 0, 3, 0))
#' sf_range <- range(ic_sf)
#' plot_boxplot_single(ic_sf, ic_week,  1,  9, this_range = sf_range)
#' plot_boxplot_single(ic_sf, ic_week, 10, 18, this_range = sf_range)
#' plot_boxplot_single(ic_sf, ic_week, 19, 27, this_range = sf_range)
#' plot_boxplot_single(ic_sf, ic_week, 28, 36, this_range = sf_range)
#' plot_boxplot_single(ic_sf, ic_week, 37, 45, this_range = sf_range)
#' plot_boxplot_single(ic_sf, ic_week, 46, 53, this_range = sf_range)
#' @import stats
#' @import graphics
#' @export
plot_boxplot_single <- function(this_factor, this_week, first_week, last_week, forecast = 0, this_range = NULL) {
    # Author: Brian C. Monsell (OEUS), Version 1.8, 1/12/2022
    
    # generate number of factors not including forecasts
    fac <- this_factor[1:(length(this_factor) - forecast)]
    
    # generate filter for the range of weeks being plotted
    this_filter <- this_week == first_week
    for (this_freq in seq((first_week + 1),last_week)) {
        this_filter <- this_filter | this_week == this_freq
    }
    
    # generate boxplots of factors
    if (!is.null(this_range)) {
        boxplot(fac[this_filter] ~ this_week[this_filter], ylim = this_range)
    } else {
        boxplot(fac[this_filter] ~ this_week[this_filter])
    }
    
}
