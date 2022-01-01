# simts 0.1.1

## Features

This R package provides a series of tools to simulate, plot, estimate, select and forecast different time series models. More specifically, it provides the following features:

- Simulation of time series from SARIMA models to various state-space models that can be expressed as latent time series processes.
- Visualization of time series data with user specifications.
- Specific simulation and visualization tools for latent time series models.
- Easy-to-use functions to estimate and infer on the parameters of time series models through different methods.
- Diagnostic and statistical tools to assess goodness of fit and select the best model for the data.
- Estimating and plotting tools to deliver point forecasts and confidence intervals.
  

## Updates

The initial release of the package (version 0.1.0) reported some problems with Solaris due to C++ code compatibilty. Some functions were misused on an integer type, which may cause the potential overloading ambiguity. In the latest version, we hope that these issues have been overcome but we remain available to correct the package should other issues arise.