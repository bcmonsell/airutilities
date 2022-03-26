#' High density plots for weekly series
#'
#' Generates high density plots for weekly time series around a given mean - current supported time series objects are xts and tis
#'
#' @param x numeric vector, original time series object to be plotted.
#' @param this_start Integer array of length 2, beginning date for plot. Default is NULL (reset to starting date of series).
#' @param this_end Integer array of length 2, ending date for plot. Default is NULL (reset to ending date of series).
#' @param this_main Character string, Main title of plot. Default is "High Density Plot".
#' @param this_range numeric vector of length 2, range of data displayed in plot. Default is NULL, the range of \code{x} will be used.
#' @param this_col character string, color of lines in plot. Default is "grey".
#' @param this_ylab character string, label for Y-axis on plot. Default is " ", or no label.
#' @param this_mu numeric scalar, mean value that the high density plot will be plotted around. Default is 0.0
#' @param this_cex numeric scalar, value for plotting parameter \code{cex} which controls the amount by which plotting text and 
#'        symbols should be scaled relative to the default. Default is 0.75.
#' @return High density plot for \code{x}
#' @examples
#' this_color <- c("#4682B4", "#7846B4", "#B47846", "#82B446")
#' par(mfrow = c(2,2), mar= c(2,4,4.25,1.0), oma=c(0,0,3,0))
#' ic_outlier_range     <- 
#'    range(ic_outlier, ic_outlier_auto, ic_outlier_auto_ljung, ic_outlier_tc)
#' weekly_high_density_plot(ic_outlier, this_start = c(2018,1), 
#'                          this_main = "default model", 
#'                          this_range = ic_outlier_range, 
#'                          this_ylab = "IC", this_col = this_color[1])
#' weekly_high_density_plot(ic_outlier_auto, this_start = c(2018,1), 
#'                          this_main = "auto outlier", 
#'                          this_range = ic_outlier_range, 
#'                          this_ylab = "IC", this_col = this_color[2])
#' weekly_high_density_plot(ic_outlier_auto_ljung, this_start = c(2018,1), 
#'                          this_main = "auto, ljung cv", 
#'                          this_range = ic_outlier_range, 
#'                          this_ylab = "IC", this_col = this_color[3])
#' weekly_high_density_plot(ic_outlier_tc, this_start = c(2018,1), 
#'                          this_main = "auto with tc", 
#'                          this_range = ic_outlier_range, 
#'                          this_ylab = "IC", this_col = this_color[4])
#' mtext("Initial Claims, outlier, from 2018", 3, 1.5, outer=TRUE)
#' @import stats
#' @import graphics
#' @export
weekly_high_density_plot <- function(x, this_start = NULL, this_end = NULL, this_main = NULL, this_range = NULL, 
                                        this_ylab = " ", this_col = "grey", this_mu = 0, this_cex = 0.75) {
    # Author: Brian C. Monsell (OEUS), Version 1.11, 3/7/2022
    
    # determine whether to shorten the series
    shorten <- 0
    if (is.null(this_start)) {
        this_start <- start(x)
        shorten <- shorten + 1
    }
    if (is.null(this_end)) {
        this_end <- end(x)
        shorten <- shorten + 1
    }
    
    this_x <- x
    if (shorten < 2) {
        this_x <- window(x, start = this_start, end = this_end)
    }
    
    # generate range of the series
    if (is.null(this_range)){
        this_range <- range(this_x)
    }
    
    # generate dates for plot
    if (xts::is.xts(this_x)) {
        this_date <- zoo::index(this_x)
    } else {
        if (tis::is.tis(this_x)) {
            this_date <- as.Date(tis::as.Date.ti(tis::ti(this_x), origin = "1970-01-01"), "%d/%m/%Y")
        }
        else {
            stop("time series must be either an xts or tis object")
        }
    }
    # generate title for main plot if it is not set
    if (is.null(this_main)) {
        this_main <- "High Density Plot"
    }

    # generate high density plot for data, with a horizontal line at this_mu
    plot(this_x~this_date, type="n", main=this_main, ylim = this_range, 
         ylab = this_ylab, xlab=" ", cex=this_cex)
    for (i in 1:length(this_x)) {
        segments(this_date[i], this_x[i], this_date[i], this_mu, col=this_col)
    }
    abline(h = this_mu)
}