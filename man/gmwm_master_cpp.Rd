% Generated by roxygen2: do not edit by hand
% Please edit documentation in src/gmwm_logic.cpp, src/gmwm_logic.h
\name{gmwm_master_cpp}
\alias{gmwm_master_cpp}
\title{Master Wrapper for the GMWM Estimator}
\usage{
gmwm_master_cpp(
  data,
  theta,
  desc,
  objdesc,
  model_type,
  starting,
  alpha,
  compute_v,
  K,
  H,
  G,
  robust,
  eff
)
}
\arguments{
\item{data}{A \code{vec} containing the data.}

\item{theta}{A \code{vec} with dimensions N x 1 that contains user-supplied initial values for parameters}

\item{desc}{A \code{vector<string>} indicating the models that should be considered.}

\item{objdesc}{A \code{field<vec>} containing a list of parameters (e.g. AR(1) = c(1,1), ARMA(p,q) = c(p,q,1))}

\item{model_type}{A \code{string} that represents the model transformation}

\item{starting}{A \code{bool} that indicates whether the supplied values are guessed (T) or are user-based (F).}

\item{alpha}{A \code{double} that handles the alpha level of the confidence interval (1-alpha)*100}

\item{compute_v}{A \code{string} that describes what kind of covariance matrix should be computed.}

\item{K}{An \code{int} that controls how many times theta is updated.}

\item{H}{An \code{int} that controls how many bootstrap replications are done.}

\item{G}{An \code{int} that controls how many guesses at different parameters are made.}

\item{robust}{A \code{bool} that indicates whether the estimation should be robust or not.}

\item{eff}{A \code{double} that specifies the amount of efficiency required by the robust estimator.}
}
\value{
A \code{field<mat>} that contains a list of ever-changing estimates...
}
\description{
This function generates WV, GMWM Estimator, and an initial test estimate.
}
\references{
Wavelet variance based estimation for composite stochastic processes, S. Guerrier and Robust Inference for Time Series Models: a Wavelet-Based Framework, S. Guerrier
}
\author{
JJB
}
\keyword{internal}
