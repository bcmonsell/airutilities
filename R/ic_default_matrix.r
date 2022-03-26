#' Regression matrix with default outlier and holiday regressors for IC
#'
#' A matrix object with the default regression matrix used for the default model of 
#' IC in the factional airline model fit to the weekly initial unemployment claims data
#'
#' @format A 940 x 69 matrix with regressors in each column. The first 10 columns are regular holidays:
#' \describe{
#'   \item{ny}{New Years Day Holiday}
#'   \item{mlk}{MLK Holiday}
#'   \item{president}{Presidents Day Holiday}
#'   \item{easter}{Easter Holiday}
#'   \item{memorial}{Memorial Day Holiday}
#'   \item{july4}{July 4th Holiday}
#'   \item{labor}{Labor Day Holiday}
#'   \item{columbus}{Columbus Day Holiday}
#'   \item{veteran}{Veteran's Day Holiday}
#'   \item{thanksgiving}{Thanksgiving Holiday}
#' }
#' The next 3 columns are special holidays:
#' \describe{
#'   \item{july4_wed}{July 4th falls on a Wednesday}
#'   \item{xmas_w53}{Christams falls in the 53rd week}
#'   \item{xmas_fri}{Christmas falls on a Friday}
#' }
#' The remaining columns are AO outliers in different weeks.
#' Every week in the pandemic has an AO outlier. The outlier list is given below:
#' AO(week 37, 2005) AO(week 38, 2005) AO(week 39, 2005) AO(week 40, 2005) 
#' AO(week 41, 2005) AO(week 1, 2006)  AO(week 2, 2007)	 AO(week 4, 2008)  
#' AO(week 45, 2012) AO(week 35, 2017) AO(week 12, 2020) AO(week 13, 2020) 
#' AO(week 14, 2020) AO(week 15, 2020) AO(week 16, 2020) AO(week 17, 2020) 
#' AO(week 18, 2020) AO(week 19, 2020) AO(week 20, 2020) AO(week 21, 2020) 
#' AO(week 22, 2020) AO(week 23, 2020) AO(week 24, 2020) AO(week 25, 2020) 
#' AO(week 26, 2020) AO(week 27, 2020) AO(week 28, 2020) AO(week 29, 2020) 
#' AO(week 30, 2020) AO(week 31, 2020) AO(week 32, 2020) AO(week 33, 2020) 
#' AO(week 34, 2020) AO(week 35, 2020) AO(week 36, 2020) AO(week 37, 2020) 
#' AO(week 38, 2020) AO(week 39, 2020) AO(week 40, 2020) AO(week 41, 2020) 
#' AO(week 42, 2020) AO(week 43, 2020) AO(week 44, 2020) AO(week 45, 2020) 
#' AO(week 46, 2020) AO(week 47, 2020) AO(week 48, 2020) AO(week 49, 2020) 
#' AO(week 50, 2020) AO(week 51, 2020) AO(week 52, 2020) AO(week 1, 2021)  
#' AO(week 2, 2021)  AO(week 3, 2021)  AO(week 4, 2021) AO(week 5, 2021)
#'
"ic_default_matrix"
