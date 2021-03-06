% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/measure_get_measures_by_year_state.R
\name{measure_get_measures_by_year_state}
\alias{measure_get_measures_by_year_state}
\title{Get a dataframe of ballot measures by year and state}
\usage{
measure_get_measures_by_year_state(
  years = lubridate::year(lubridate::today()),
  state_ids = state.abb,
  all = TRUE,
  verbose = TRUE
)
}
\arguments{
\item{years}{A vector of election years.}

\item{state_ids}{A vector of state abbreviations.}

\item{all}{Boolean: should all possible combinations of the variables be searched for, or just the exact combination of them in the order they are supplied?}

\item{verbose}{Should cases when no data is available be messaged?}
}
\value{
A dataframe of ballot measures and their attributes. If a given \code{year} + \code{state_id} returns no data, that row will be filled with \code{NA}s.
}
\description{
More information about these ballot measures can be found using the \code{\link{measure_get_measures}} function.
}
\examples{
\dontrun{
measure_get_measures_by_year_state(years = c(2016, 2018), state_ids = c("MO", "IL", "VT"))
}
}
