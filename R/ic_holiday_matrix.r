#' Regression matrix with default holiday regressors for IC
#'
#' A matrix object with the default holiday regression matrix used for the default model of 
#' IC in the factional airline model fit to the weekly initial unemployment claims data
#'
#' @format A 940 x 13 matrix with regressors in each column. The first 10 columns are regular holidays:
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
#'
"ic_holiday_matrix"
