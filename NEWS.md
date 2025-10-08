# simts 0.2.3

- solved problems related to documentation with itemize tags in return tags that were creating problems on check
- moved from arma::is_finite to std::isfinite in file src/rtoarmadillo.cpp

# simts 0.2.0

## Features

This R package provides a series of tools to simulate, plot, estimate, select and forecast different time series models. More specifically, it provides the following features:

- Simulation of time series from SARIMA models to various state-space models that can be expressed as latent time series processes.
- Visualization of time series data with user specifications.
- Specific simulation and visualization tools for latent time series models.
- Easy-to-use functions to estimate and infer on the parameters of time series models through different methods.
- Diagnostic and statistical tools to assess goodness of fit and select the best model for the data.
- Estimating and plotting tools to deliver point forecasts and confidence intervals.
  

## Updates

### version 0.1.0

The initial release of the package (version 0.1.0) reported some problems with Solaris due to C++ code compatibilty. Some functions were misused on an integer type, which may cause the potential overloading ambiguity. In the latest version, we hope that these issues have been overcome but we remain available to correct the package should other issues arise.

### version 0.2.0

- Correct LaTeX errors in the documentation for SIN process.
- Removed `sales` dataset and update vignette examples accordingly.
- Added GitHub Actions CI/CD workflow for package check.
- Specified C++11 compilation for portability.

### version 0.2.1

- Added support for generating Matérn process
- Added support for generating fractional Gaussian noise
- Added support for generating Power Law Process
- re-generate vignettes roxygen2 v.7.2.1 for HTML5 standard compatibility

