% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/optim-prediction-statistics-function.R
\name{optim.prediction.statistics}
\alias{optim.prediction.statistics}
\title{Compute prediction statistics}
\usage{
See demo
}
\arguments{
\item{sim}{is the simulated data}

\item{df}{is the dataframe with the data to which the model will be fitted}

\item{P}{is the confidence intercal (0.95 means 95\%)}

\item{ylim}{is a vecor of two elements indicating the min and max of the output.}

\item{na.rm}{controls if NA-values should be removed}

\item{trace}{controls if the function should be verbose during execution.}
}
\value{
Prediction statistics based on the simulated data
}
\description{
Compute prediction statistics for model fitting in clanOptim.
}
\author{
Claus E. Andersen
}
