% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/match_week.r
\name{match_week}
\alias{match_week}
\title{Match a specific week}
\usage{
match_week(x_week, week_number)
}
\arguments{
\item{x_week}{Numeric tis object.}

\item{week_number}{Numeric scalar; Week of the year to match.}
}
\value{
An indicator variable where the observation that matches the week entered = 1, 0 otherwise.
}
\description{
Create an indicator variable for a specific week.
}
\examples{
xmas_w53 <- match_week(ic_week, 53)
}
