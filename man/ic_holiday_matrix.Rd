% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ic_holiday_matrix.r
\docType{data}
\name{ic_holiday_matrix}
\alias{ic_holiday_matrix}
\title{Regression matrix with default holiday regressors for IC}
\format{
A 940 x 13 matrix with regressors in each column. The first 10 columns are regular holidays:
\describe{
\item{ny}{New Years Day Holiday}
\item{mlk}{MLK Holiday}
\item{president}{Presidents Day Holiday}
\item{easter}{Easter Holiday}
\item{memorial}{Memorial Day Holiday}
\item{july4}{July 4th Holiday}
\item{labor}{Labor Day Holiday}
\item{columbus}{Columbus Day Holiday}
\item{veteran}{Veteran's Day Holiday}
\item{thanksgiving}{Thanksgiving Holiday}
}
The next 3 columns are special holidays:
\describe{
\item{july4_wed}{July 4th falls on a Wednesday}
\item{xmas_w53}{Christams falls in the 53rd week}
\item{xmas_fri}{Christmas falls on a Friday}
}
}
\usage{
ic_holiday_matrix
}
\description{
A matrix object with the default holiday regression matrix used for the default model of
IC in the factional airline model fit to the weekly initial unemployment claims data
}
\keyword{datasets}
