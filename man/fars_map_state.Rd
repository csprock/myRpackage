% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{fars_map_state}
\alias{fars_map_state}
\title{Plots the locations of accidents on a state map}
\usage{
fars_map_state(state.num, year)
}
\arguments{
\item{state.num}{number of the state in the FARS dataset, must be set}

\item{year}{the year, must be set of filename is NULL}
}
\value{
\code{NULL} if no data, else shows plot
}
\description{
Plots the locations of accidents on a state map
}
\details{
This function takes number of a state in the FARS dataset along with a year
and uses the \code{maps} package to plot the locations of each accident on a
map of the state.

This function requires \code{dplyr} and \code{maps}
}
\section{Errors}{

Raises an error of the state number does not match that
of any in the FARS dataset
}

\examples{
\dontrun{fars_map_state(1, 2013)}

}
