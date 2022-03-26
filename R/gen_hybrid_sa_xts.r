#' Generate a hybrid seasonal adjustment of an xts time series object
#'
#' Generates a "hybrid" seasonal adjustment by replacing a span of a multiplicative seasonal adjustment with an additive adjustment
#'
#' @param this_mult_sa xts time series object of a multiplicative seasonal adjustment
#' @param this_add_sa xts time series object of an additive seasonal adjustment
#' @param this_start_hybrid integer vector of length 2, start of the span where additive adjustments replace multiplicative adjustment, defined as \code{c(year, week)}
#' @param this_end_hybrid integer vector of length 2, end of the span where additive adjustments replace multiplicative adjustment, defined as \code{c(year, week)}
#' @param this_week_xts Numeric vector; Week of the year for each observation, stored as an \code{xts} time series object.
#' @param this_year_xts Numeric vector; Year of each observation, stored as an \code{xts} time series object.
#' @return xts series object with hybrid seasonal adjustment
#' @examples
#' cc_hybrid_sa <- gen_hybrid_sa_xts(cc_sa_tc_log, cc_sa_tc_nolog, c(2020,12), c(2020, 52),
#'                                   cc_week_xts, cc_year_xts)
#' @export
gen_hybrid_sa_xts <- function(this_mult_sa, this_add_sa, this_start_hybrid, this_end_hybrid, 
                              this_week_xts, this_year_xts) {
    # Author: Brian C. Monsell (OEUS) Version 1.7, 3/14/2022
    this_start <- start(this_mult_sa)
    this_end   <- end(this_mult_sa)
    this_index  <- as.Date(zoo::index(this_mult_sa), origin = "1970-01-01")
    
    # set starting date to the 5th week in 2004
    start_filter          <- this_week_xts == this_start_hybrid[2] & this_year_xts == this_start_hybrid[1]
    this_start_hybrid_xts <- this_index[start_filter]

    # set ending date to 5th week in 2022
    end_filter            <- this_week_xts == this_end_hybrid[2] & this_year_xts == this_end_hybrid[1]
    this_end_hybrid_xts   <- this_index[end_filter]
    
    before_hybrid_start <- this_start_hybrid_xts - 7    
    after_hybrid_end <- this_end_hybrid_xts + 7
    
    span1_xts <- window(this_mult_sa, end = before_hybrid_start)
    span2_xts <- window(this_add_sa, start = this_start_hybrid_xts, end = this_end_hybrid_xts)
    span3_xts <- window(this_mult_sa, start = after_hybrid_end)
    
    this_hybrid_sa_xts <- 
       xts::xts(c(span1_xts, span2_xts, span3_xts), 
         order.by = this_index)         

    return(this_hybrid_sa_xts)
}