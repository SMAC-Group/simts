% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcppExports.R
\name{optimism_bootstrapper}
\alias{optimism_bootstrapper}
\title{Bootstrap for Optimism}
\usage{
optimism_bootstrapper(
  theta,
  desc,
  objdesc,
  scales,
  model_type,
  N,
  robust,
  eff,
  alpha,
  H
)
}
\arguments{
\item{theta}{A \code{vector} with dimensions N x 1 that contains user-supplied initial values for parameters}

\item{desc}{A \code{vector<string>} indicating the models that should be considered.}

\item{objdesc}{A \code{field<vec>} that contains an object description (e.g. values) of the model.}
}
\value{
A \code{vec} that contains the parameter estimates from GMWM estimator.
}
\description{
Using the bootstrap approach, we simulate a model based on user supplied parameters, obtain the wavelet variance, and then V.
}
\details{
Expand in detail...
}
\author{
JJB
}
\keyword{internal}
