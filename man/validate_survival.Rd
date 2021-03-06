% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/validation.R
\name{validate_survival}
\alias{validate_survival}
\title{Returns the Kaplan Meier curve comparing COPD and non-COPD}
\usage{
validate_survival(savePlots = FALSE, base_agents = 10000)
}
\arguments{
\item{savePlots}{TRUE or FALSE (default), exports 300 DPI population growth and pyramid plots comparing simulated vs. predicted population}

\item{base_agents}{Number of agents in the simulation. Default is 1e4.}
}
\value{
validation test results
}
\description{
Returns the Kaplan Meier curve comparing COPD and non-COPD
}
