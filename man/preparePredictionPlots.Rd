% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotUtilities.R
\name{preparePredictionPlots}
\alias{preparePredictionPlots}
\title{Prepare population OR individual prediction plots.
This includes the prediction plot and the timeline plot.}
\usage{
preparePredictionPlots(
  populationPredict,
  individualPredict,
  observed,
  target,
  model,
  now,
  regimen
)
}
\arguments{
\item{populationPredict}{dataframe containing the individual prediction data}

\item{individualPredict}{dataframe containing the population prediction data}

\item{observed}{a data.frame with the following columns:
\itemize{
\item \code{time} a POSIXct time
\item \code{dv} the observed data
\item \code{use} whether the observation should be used in the estimation
}}

\item{target}{a named list with min/max values to represent the target concentration}

\item{model}{a tdmore model with (ideally) metadata}

\item{now}{a POSIXct time representing the current time}

\item{regimen}{a data.frame with the following columns:
\itemize{
\item \code{time} a POSIXct time
\item \code{dose} a numeric with the dose amount
\item \code{formulation} a character vector that corresponds to formulations defined in the metadata of the model
\item \code{fix} a boolean vector describing whether the given regimen can be modified in dose recommendation
}}
}
\value{
a list of plots (p1, p2, p3)
}
\description{
Prepare population OR individual prediction plots.
This includes the prediction plot and the timeline plot.
}
