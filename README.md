
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![R-CMD-check](https://github.com/SMAC-Group/simts/workflows/R-CMD-check/badge.svg)](https://github.com/SMAC-Group/simts/actions)
[![Licence](https://img.shields.io/badge/licence-AGPL--3.0-blue.svg)](https://opensource.org/licenses/AGPL-3.0)
[![minimal R
version](https://img.shields.io/badge/R%3E%3D-3.4.0-6666ff.svg)](https://cran.r-project.org/)
[![CRAN](http://www.r-pkg.org/badges/version/simts)](https://cran.r-project.org/package=simts)
[![CRAN RStudio mirror
downloads](http://cranlogs.r-pkg.org/badges/simts)](https://www.r-pkg.org/pkg/simts)
[![CRAN RStudio mirror
downloads](https://cranlogs.r-pkg.org/badges/grand-total/simts)](https://www.r-pkg.org/pkg/simts)
[![Last-changedate](https://img.shields.io/badge/last%20change-2022--01--03-green.svg)](https://github.com/SMAC-Group/simts)

# `simts` Overview <a href="https://smac-group.com/"><img src="man/figures/logo.png" align="right" style="width: 20%; height: 20%"/></a>

The Time Series Tools (`simts`) R package provides a series of tools to
simulate, plot, estimate, select and forecast different time series
models. Its original purpose was to be a support to the online textbook
[“Applied Time Series Analysis with
R”](https://smac-group.github.io/ts/) but can obviously be used for time
series analysis in general. More specifically, the package provides
tools with the following features:

-   Simulation of time series from SARIMA models to various state-space
    models that can be expressed as latent time series processes.
-   Visualization of time series data with user specifications.
-   Specific simulation and visualization tools for latent time series
    models.
-   Easy-to-use functions to estimate and infer on the parameters of
    time series models through different methods (standard and robust).
-   Diagnostic and statistical tools to assess goodness of fit and
    select the best model for the data.
-   Estimating and plotting tools to deliver point forecasts and
    confidence intervals.

To understand the usage of the `simts` package, please refer to the
“Vignettes” tab above.

## Install Instructions

## Installation

The `simts` package is available on both CRAN and GitHub. The CRAN
version is considered stable while the GitHub version is subject to
modifications/updates which may lead to installation problems or broken
functions. You can install the stable version of the `simts` package
with:

``` r
install.packages("simts")
```

For users who are interested in having the latest developments, the
GitHub version is ideal although more dependencies are required to run a
stable version of the package. Most importantly, users **must** have a
(C++) compiler installed on their machine that is compatible with R
(e.g. Clang).

``` r
# Install dependencies
install.packages(c("RcppArmadillo","devtools","knitr","rmarkdown"))

# Install the package from GitHub without Vignettes/User Guides
devtools::install_github("SMAC-Group/simts")

# Install the package with Vignettes/User Guides 
devtools::install_github("SMAC-Group/simts", build_vignettes = TRUE)
```

*The setup to obtain the development version of `simts` is platform
dependent.*

## License

The license this source code is released under is the GNU AFFERO GENERAL
PUBLIC LICENSE (AGPL) v3.0. Please see the LICENSE file for full text.
Otherwise, please consult [TLDR
Legal](https://tldrlegal.com/license/gnu-affero-general-public-license-v3-(agpl-3.0))
or [GNU](https://www.gnu.org/licenses/agpl-3.0.en.html) which will
provide a synopsis of the restrictions placed upon the code.

<!-- ### Requirements and Dependencies -->
<!-- **OS X** -->
<!-- Some users report the need to use X11 to suppress shared library errors. To install X11, visit [xquartz.org](http://www.xquartz.org/). -->
<!-- **Linux** -->
<!-- Both curl and libxml are required. -->
<!-- For **Debian** systems, enter the following in terminal: -->
<!-- ```{r, eval = F, engine='bash'} -->
<!-- sudo apt-get install curl libcurl3 libcurl3-dev libxml2 libxml2-dev -->
<!-- ``` -->
<!-- For **RHEL** systems, enter the following in terminal: -->
<!-- ```{r, eval = F, engine='bash'} -->
<!-- sudo yum install curl curl-devel libxml2 libxml2-dev -->
<!-- ``` -->
